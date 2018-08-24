open WalletTypes;

type config = {subdomain: string};

let testnetConfig = {subdomain: "testnet-api"};

let mainnetConfig = {subdomain: "api"};

let float_ = Json.Decode.float;

let decodeUTXO = raw : WalletTypes.utxo =>
  Json.Decode.{
    txId: raw |> field("txid", string),
    txOutputN: raw |> field("n", int),
    amount: raw |> field("value_int", float_) |> BTC.fromSatoshisFloat,
    confirmations: raw |> field("confirmations", int),
    address: field("addresses", array(string), raw)[0],
  };

let decodeUTXOs = raw =>
  Json.Decode.(
    raw
    |> withDefault([], field("unspent", withDefault([], list(decodeUTXO))))
  );

let decodeTransaction = raw =>
  Json.Decode.{
    txId: raw |> field("txid", string),
    blockHeight: raw |> field("block", optional(float_)),
    unixTime: raw |> field("time", optional(float_)),
  };

let decodeTransactions = raw =>
  Json.Decode.(
    raw
    |> withDefault(
         [],
         field("transactions", withDefault([], list(decodeTransaction))),
       )
  );
let decodeTransactionHexs = raw =>
  Json.Decode.(
    raw
    |> withDefault(
         [],
         field(
           "hex",
           withDefault(
             [],
             list(raw =>
               (raw |> field("txid", string), raw |> field("hex", string))
             ),
           ),
         ),
       )
  );

let decodeNextLink = raw =>
  Json.Decode.(
    raw |> optional(field("paging", field("next_link", string)))
  );

let rec fetchAll = (link, decoder, collector) =>
  Js.Promise.(
    switch (link) {
    | Some(link) =>
      Fetch.fetch(link)
      |> then_(Fetch.Response.json)
      |> then_(res =>
           fetchAll(
             decodeNextLink(res),
             decoder,
             collector |> List.append(decoder(res)),
           )
         )
    | None => resolve(collector)
    }
  );

let getUTXOs = (config, addresses) =>
  switch (addresses) {
  | [] => Js.Promise.resolve(WalletTypes.emptyUtxoSet)
  | addresses =>
    fetchAll(
      Some(
        "https://"
        ++ config.subdomain
        ++ ".smartbit.com.au/v1/blockchain/address/"
        ++ List.fold_left((res, a) => a ++ "," ++ res, "", addresses)
        ++ "/unspent?limit=1000",
      ),
      decodeUTXOs,
      [],
    )
    |> Js.Promise.then_(utxos =>
         utxos
         |. Belt.List.toArray
         |> Belt.Set.mergeMany(WalletTypes.emptyUtxoSet)
         |> Js.Promise.resolve
       )
  };

let getTransactionInfo = (config, transactions) =>
  if (transactions |> Belt.Set.String.isEmpty) {
    Js.Promise.resolve([]);
  } else {
    fetchAll(
      Some(
        "https://"
        ++ config.subdomain
        ++ ".smartbit.com.au/v1/blockchain/tx/"
        ++ Belt.Set.String.reduceU(transactions, "", (. res, a) =>
             a ++ "," ++ res
           ),
      ),
      decodeTransactions,
      [],
    );
  };
let getTransactionHex = (config, transactions) =>
  if (transactions |> Belt.Array.size == 0) {
    Js.Promise.resolve([||]);
  } else {
    fetchAll(
      Some(
        "https://"
        ++ config.subdomain
        ++ ".smartbit.com.au/v1/blockchain/tx/"
        ++ Belt.Array.reduceU(transactions, "", (. res, a) =>
             a ++ "," ++ res
           )
        ++ "/hex",
      ),
      decodeTransactionHexs,
      [],
    )
    |> Js.Promise.then_(list_ =>
         list_ |> Belt.List.toArray |> Js.Promise.resolve
       );
  };

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
           | Some(err) =>
             if (Js.Re.test(err, [%re "/transaction already in block chain/"])) {
               WalletTypes.AlreadyInBlockchain;
             } else {
               WalletTypes.Error(err);
             }
           | None =>
             WalletTypes.Ok(Json.Decode.(res |> field("txid", string)))
           }
         )
         |> resolve;
       })
    |> catch(err => WalletTypes.FetchError(err) |> resolve)
  );
};

let getCurrentBlockHeight = (config, ()) =>
  Js.Promise.(
    Fetch.fetch(
      "https://"
      ++ config.subdomain
      ++ ".smartbit.com.au/v1/blockchain/blocks?sort=height",
    )
    |> then_(Fetch.Response.json)
    |> then_(res => {
         let height =
           Json.Decode.(
             res
             |> field(
                  "blocks",
                  array(block => block |> field("height", int)),
                )
             |. Belt.Array.getExn(0)
           );
         height |> resolve;
       })
  );

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
