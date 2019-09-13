open Belt;

type config = {url: string};
let testnetConfig = {url: "https://blockstream.info/testnet/api"};
let mainnetConfig = {url: "https://blockstream.info/api"};

let float_ = Json.Decode.float;

let decodeUTXO = (address, raw): WalletTypes.utxo =>
  Json.Decode.{
    txId: raw |> field("txid", string),
    txOutputN: raw |> field("vout", int),
    amount: raw |> field("value", float_) |> BTC.fromSatoshisFloat,
    address,
  };

let decodeUTXOs = (address, raw) =>
  Json.Decode.(raw |> withDefault([], list(decodeUTXO(address))));

let getUTXOs = (config, addresses) =>
  switch (addresses) {
  | [] => Js.Promise.resolve(WalletTypes.emptyUtxoSet)
  | addresses =>
    Js.Promise.(
      addresses
      ->(
          List.map(address =>
            Fetch.fetch(config.url ++ "/address/" ++ address ++ "/utxo")
            |> then_(Fetch.Response.json)
            |> then_(raw => {
                 Js.log(raw);
                 raw |> decodeUTXOs(address) |> resolve;
               })
          )
        )
      |> List.toArray
      |> all
      |> then_(results =>
           results
           ->(
               Array.reduce(WalletTypes.emptyUtxoSet, (res, utxos) =>
                 res->(Set.mergeMany(utxos |> List.toArray))
               )
             )
           |> resolve
         )
    )
  };

let getTransactionInfo = (config, transactions) =>
  Js.Promise.(
    all(
      transactions
      ->(
          Set.String.reduceU([], (. res, txId) =>
            [
              Fetch.fetch(config.url ++ "/tx/" ++ txId)
              |> then_(Fetch.Response.json)
              |> then_(raw =>
                   (
                     Json.Decode.{
                       txId,
                       blockHeight:
                         raw
                         |> field(
                              "status",
                              optional(field("block_height", float_)),
                            ),
                       unixTime:
                         raw
                         |> field(
                              "status",
                              optional(field("block_time", float_)),
                            ),
                     }: WalletTypes.txInfo
                   )
                   |> resolve
                 ),
              ...res,
            ]
          )
        )
      |> List.toArray,
    )
    |> then_(res => res |> List.fromArray |> resolve)
  );
let getCurrentBlockHeight = (config, ()) =>
  Js.Promise.(
    Fetch.fetch(config.url ++ "/blocks/tip/height")
    |> then_(Fetch.Response.json)
    |> then_(res => res |> Json.Decode.int |> resolve)
  );

let getTransactionHex = (config, transactions) =>
  Js.Promise.(
    all(
      transactions
      ->(
          Array.mapU((. txId) =>
            Fetch.fetch(config.url ++ "/tx/" ++ txId ++ "/hex")
            |> then_(Fetch.Response.text)
            |> then_(hex => (txId, hex) |> resolve)
          )
        ),
    )
  );
let broadcastTransaction = (config, transaction) => {
  let txHex = transaction |> Bitcoin.Transaction.toHex;
  let txId = transaction |> Bitcoin.Transaction.getId;
  Js.Promise.(
    Fetch.fetchWithInit(
      config.url ++ "/tx",
      Fetch.RequestInit.make(
        ~method_=Fetch.Post,
        ~body=Fetch.BodyInit.make(txHex),
        (),
      ),
    )
    |> then_(Fetch.Response.text)
    |> then_(res =>
         if (res == txId) {
           WalletTypes.Ok(txId) |> resolve;
         } else {
           WalletTypes.Error(res) |> resolve;
         }
       )
    |> catch(err => WalletTypes.FetchError(err) |> resolve)
  );
};

let make = (config, network): (module WalletTypes.NetworkClientInterface) =>
  (module
   {
     let network = network;
     let getUTXOs = getUTXOs(config);
     let getTransactionInfo = getTransactionInfo(config);
     let getTransactionHex = getTransactionHex(config);
     let getCurrentBlockHeight = getCurrentBlockHeight(config);
     let broadcastTransaction = broadcastTransaction(config);
   });
