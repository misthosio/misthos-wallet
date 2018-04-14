type config = {subdomain: string};

let testnetConfig = {subdomain: "testnet-api"};

let mainnetConfig = {subdomain: "api"};

let float_ = Json.Decode.float;

let decodeUTXO = (address, raw) : WalletTypes.utxo =>
  Json.Decode.{
    txId: raw |> field("txid", string),
    txOutputN: raw |> field("n", int),
    amount:
      raw |> field("value_int", float_) |> Int64.of_float |> BTC.fromSatoshis,
    confirmations: raw |> field("confirmations", int),
    address,
  };

let decodeUTXOs = (address, raw) =>
  Json.Decode.(
    raw
    |> withDefault(
         [],
         field("unspent", withDefault([], list(decodeUTXO(address)))),
       )
  );

let decodeNextLink = raw =>
  Json.Decode.(
    raw |> optional(field("paging", field("next_link", string)))
  );

let rec fetchAll = (address, link, utxos) =>
  Js.Promise.(
    switch (link) {
    | Some(link) =>
      Fetch.fetch(link)
      |> then_(Fetch.Response.json)
      |> then_(res =>
           fetchAll(
             address,
             decodeNextLink(res),
             utxos |> List.append(decodeUTXOs(address, res)),
           )
         )
    | None => resolve(utxos)
    }
  );

let getUTXOs = (config, addresses) =>
  Js.Promise.(
    addresses
    |> List.map(address =>
         fetchAll(
           address,
           Some(
             "https://"
             ++ config.subdomain
             ++ ".smartbit.com.au/v1/blockchain/address/"
             ++ address
             ++ "/unspent?limit=1000",
           ),
           [],
         )
       )
    |> Array.of_list
    |> all
    |> then_(all => all |> Array.to_list |> List.flatten |> resolve)
  );

let broadcastTransaction = (config, transaction) => {
  let txHex = transaction |> Bitcoin.Transaction.toHex;
  Js.Promise.(
    Fetch.fetchWithInit(
      "https://" ++ config.subdomain ++ ".smartbit.com.au/v1/blockchain/pushtx",
      Fetch.RequestInit.make(
        ~method_=Fetch.Post,
        ~body=Fetch.BodyInit.make({j|{"hex":"$(txHex)"}|j}),
        (),
      ),
    )
    |> then_(Fetch.Response.json)
    |> then_(res => {
         let err =
           Json.Decode.(
             res |> optional(field("error", field("message", string)))
           );
         (
           switch (err) {
           | Some(err) => WalletTypes.Error(err)
           | None =>
             WalletTypes.Ok(Json.Decode.(res |> field("txid", string)))
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
