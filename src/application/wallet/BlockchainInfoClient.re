[%bs.raw {| require('formdata-polyfill') |}];

open Belt;

type config = {
  subdomain: string,
  network: Bitcoin.Networks.t,
};
let testnetConfig = {
  subdomain: "testnet.",
  network: Bitcoin.Networks.testnet,
};
let mainnetConfig = {subdomain: "", network: Bitcoin.Networks.bitcoin};

let float_ = Json.Decode.float;

let decodeUTXO = (config, raw) : WalletTypes.utxo =>
  Json.Decode.{
    txId: raw |> field("tx_hash_big_endian", string),
    txOutputN: raw |> field("tx_output_n", int),
    amount: raw |> field("value", float_) |> BTC.fromSatoshisFloat,
    confirmations: raw |> field("confirmations", int),
    address:
      raw
      |> field("script", string)
      |> Utils.bufFromHex
      |. Bitcoin.Address.fromOutputScript(config.network),
  };

let getUTXOs = (config, addresses) =>
  switch (addresses) {
  | [] => Js.Promise.resolve(WalletTypes.emptyUtxoSet)
  | addresses =>
    Js.Promise.(
      Fetch.fetch(
        "https://"
        ++ config.subdomain
        ++ "blockchain.info/unspent?format=json&active="
        ++ (
          addresses
          |. List.reduceU("", (. res, address) => res ++ "|" ++ address)
        )
        ++ "&cors=true",
      )
      |> then_(Fetch.Response.json)
      |> then_(raw =>
           raw
           |> Json.Decode.(
                field("unspent_outputs", array(decodeUTXO(config)))
              )
           |> Set.mergeMany(WalletTypes.emptyUtxoSet)
           |> resolve
         )
      |> catch(_err => WalletTypes.emptyUtxoSet |> resolve)
    )
  };
let getTransactionInfo = (config, transactions) =>
  Js.Promise.(
    all(
      transactions
      |. Set.String.reduceU([], (. res, txId) =>
           [
             Fetch.fetch(
               "https://"
               ++ config.subdomain
               ++ "blockchain.info/rawtx/"
               ++ txId
               ++ "?format=json&cors=true",
             )
             |> then_(Fetch.Response.json)
             |> then_(raw =>
                  (
                    Json.Decode.{
                      txId,
                      blockHeight:
                        raw |> optional(field("block_height", float_)),
                      unixTime: raw |> optional(field("time", float_)),
                    }: WalletTypes.txInfo
                  )
                  |> resolve
                ),
             ...res,
           ]
         )
      |> List.toArray,
    )
    |> then_(res => res |> List.fromArray |> resolve)
  );
let getTransactionHex = (config, transactions) =>
  Js.Promise.(
    all(
      transactions
      |. Array.mapU((. txId) =>
           Fetch.fetch(
             "https://"
             ++ config.subdomain
             ++ "blockchain.info/rawtx/"
             ++ txId
             ++ "?format=hex&cors=true",
           )
           |> then_(Fetch.Response.text)
           |> then_(hex => (txId, hex) |> resolve)
         ),
    )
  );
let getCurrentBlockHeight = (config, ()) =>
  Js.Promise.(
    Fetch.fetch(
      "https://" ++ config.subdomain ++ "blockchain.info/latestblock?cors=true",
    )
    |> then_(Fetch.Response.json)
    |> then_(res => {
         let height = Json.Decode.(res |> field("height", int));
         height |> resolve;
       })
  );
let broadcastTransaction = (config, transaction) => {
  let txHex = transaction |> Bitcoin.Transaction.toHex;
  let formData = FormData.make();
  formData |. FormData.append("tx", txHex);
  Js.Promise.(
    Fetch.fetchWithInit(
      "https://" ++ config.subdomain ++ "blockchain.info/pushtx?cors=true",
      Fetch.RequestInit.make(
        ~method_=Fetch.Post,
        ~body=Fetch.BodyInit.makeWithFormData(formData),
        (),
      ),
    )
    |> then_(Fetch.Response.text)
    |> then_(responseText => {
         let responseText = responseText |> Js.String.toLowerCase;
         (
           responseText |> Js.String.indexOf("transaction submitted") >= 0 ?
             Ok(transaction |> Bitcoin.Transaction.getId) :
             responseText
             |> Js.String.indexOf("transaction already exists") >= 0 ?
               AlreadyInBlockchain : Error(responseText): WalletTypes.broadcastResult
         )
         |> resolve;
       })
  );
};

let make = (config, network) : (module WalletTypes.NetworkClientInterface) =>
  (module
   {
     let network = network;
     let getUTXOs = getUTXOs(config);
     let getTransactionInfo = getTransactionInfo(config);
     let getTransactionHex = getTransactionHex(config);
     let getCurrentBlockHeight = getCurrentBlockHeight(config);
     let broadcastTransaction = broadcastTransaction(config);
   });
