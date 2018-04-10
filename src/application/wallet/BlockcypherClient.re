type config = {network: string};

let decodeUTXO = (address, raw) : WalletTypes.utxo =>
  Json.Decode.{
    txId: raw |> field("tx_hash", string),
    txOutputN: raw |> field("tx_output_n", int),
    amount: raw |> field("value", float) |> Int64.of_float |> BTC.fromSatoshis,
    confirmations: raw |> field("confirmations", int),
    address
  };

let decodeUTXOs = (address, raw) =>
  Json.Decode.(
    raw
    |> withDefault(
         [],
         field("txrefs", withDefault([], list(decodeUTXO(address))))
       )
    |> List.append(
         raw
         |> withDefault(
              [],
              field(
                "unconfirmed_txrefs",
                withDefault([], list(decodeUTXO(address)))
              )
            )
       )
  );

let getUTXOs = (config, addresses) =>
  Js.Promise.(
    addresses
    |> List.map(address =>
         Fetch.fetch(
           "https://api.blockcypher.com/v1/btc/"
           ++ config.network
           ++ "/addrs/"
           ++ address
         )
         |> then_(Fetch.Response.json)
         |> then_(res => decodeUTXOs(address, res) |> resolve)
       )
    |> Array.of_list
    |> all
    |> then_(all => all |> Array.to_list |> List.flatten |> resolve)
  );

let broadcastTransaction = (config, transaction) => {
  let txHex = transaction |> Bitcoin.Transaction.toHex;
  Js.Promise.(
    Fetch.fetchWithInit(
      "https://api.blockcypher.com/v1/btc/" ++ config.network ++ "/txs/push",
      Fetch.RequestInit.make(
        ~method_=Fetch.Post,
        ~body=Fetch.BodyInit.make({j|{"tx":"$(txHex)"}}|j}),
        ()
      )
    )
    |> then_(Fetch.Response.json)
    |> then_(res => {
         let err = Json.Decode.(res |> optional(field("error", string)));
         (
           switch err {
           | Some(err) => WalletTypes.Error(err)
           | None => WalletTypes.Ok(Json.Decode.(res |> field("hash", string)))
           }
         )
         |> resolve;
       })
  );
};

let make = (config, network) : (module WalletTypes.NetworkClient) =>
  (module
   {
     let network = network;
     let getUTXOs = getUTXOs(config);
     let broadcastTransaction = broadcastTransaction(config);
   });
