type config = {network: string};

let decodeUTXOs = (address, raw) =>
  Json.Decode.(
    raw
    |> field(
         "txrefs",
         withDefault(
           [],
           list(utxo =>
             (
               {
                 txId: utxo |> field("tx_hash", string),
                 txOutputN: utxo |> field("tx_output_n", int),
                 amount:
                   utxo
                   |> field("value", float)
                   |> Int64.of_float
                   |> BTC.fromSatoshis,
                 confirmations: utxo |> field("confirmations", int),
                 address
               }: WalletTypes.utxo
             )
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
    |> then_(raw =>
         WalletTypes.Ok(raw |> Json.Decode.(field("hash", string))) |> resolve
       )
  );
};

let make = (config, network) : (module WalletTypes.NetworkClient) =>
  (module
   {
     let network = network;
     let getUTXOs = getUTXOs(config);
     let broadcastTransaction = broadcastTransaction(config);
   });
