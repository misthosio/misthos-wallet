let satoshisPerBTC = 1e8;

let float_ = Json.Decode.float;

type config = {
  bitcoindUrl: string,
  rpcUser: string,
  rpcPassword: string,
};

let makeAuthHeaders = ({rpcUser, rpcPassword}) => {
  let authString =
    Node_buffer.fromString({j|$(rpcUser):$(rpcPassword)|j})
    |> BufferExt.toStringWithEncoding("base64");
  Fetch.HeadersInit.make({"Authorization": {j|Basic $(authString)|j}});
};

let rpcCall = ({bitcoindUrl} as config, jsonRPC) =>
  Js.Promise.(
    Fetch.fetchWithInit(
      bitcoindUrl,
      Fetch.RequestInit.make(
        ~method_=Fetch.Post,
        ~headers=makeAuthHeaders(config),
        ~body=Fetch.BodyInit.make(jsonRPC),
        (),
      ),
    )
    |> then_(Fetch.Response.json)
  );

let importAllAs = (config, addresses, label) =>
  switch (addresses) {
  | [] => Js.Promise.resolve()
  | [first, ...rest] =>
    let jsonRPCImport =
      Json.Encode.(
        object_([
          ("jsonrpc", string("1.0")),
          ("method", string("importaddress")),
          ("params", list(string, [first, label])),
        ])
      )
      |> Json.stringify;
    let start = rpcCall(config, jsonRPCImport);
    Js.Promise.(
      rest
      |> List.fold_left(
           (p, address) =>
             p
             |> then_(_r => {
                  let jsonRPCImport =
                    Json.Encode.(
                      object_([
                        ("jsonrpc", string("1.0")),
                        ("method", string("importaddress")),
                        ("params", list(string, [address, label])),
                      ])
                    )
                    |> Json.stringify;
                  rpcCall(config, jsonRPCImport);
                }),
           start,
         )
      |> then_(_ => resolve())
    );
  };

let getUTXOs = (config, addresses) : Js.Promise.t(list(WalletTypes.utxo)) =>
  Js.Promise.(
    importAllAs(config, addresses, "")
    |> then_(_ => {
         let jsonRPCUnspent =
           Json.Encode.(
             object_([
               ("jsonrpc", string("1.0")),
               ("method", string("listunspent")),
               (
                 "params",
                 tuple3(int, int, list(string), (1, 9999999, addresses)),
               ),
             ])
           )
           |> Json.stringify;
         rpcCall(config, jsonRPCUnspent);
       })
    |> then_(obj =>
         Json.Decode.(
           obj
           |> field(
                "result",
                withDefault(
                  [],
                  list(utxo =>
                    (
                      {
                        txId: utxo |> field("txid", string),
                        txOutputN: utxo |> field("vout", int),
                        address: utxo |> field("address", string),
                        amount:
                          utxo |> field("amount", float_) |> BTC.fromFloat,
                        confirmations: utxo |> field("confirmations", int),
                      }: WalletTypes.utxo
                    )
                  ),
                ),
              )
         )
         |> resolve
       )
  );

let broadcastTransaction = (config, transaction) => {
  let jsonRPC =
    Json.Encode.(
      object_([
        ("jsonrpc", string("1.0")),
        ("method", string("sendrawtransaction")),
        (
          "params",
          list(string, [transaction |> Bitcoin.Transaction.toHex]),
        ),
      ])
    )
    |> Json.stringify;
  Js.Promise.(
    rpcCall(config, jsonRPC)
    |> then_(res => {
         let err =
           Json.Decode.(
             res |> field("error", optional(field("message", string)))
           );
         (
           switch (err) {
           | Some(err) => WalletTypes.Error(err)
           | None =>
             WalletTypes.Ok(Json.Decode.(res |> field("result", string)))
           }
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
     let getTransactionInfo = _ => Js.Promise.resolve([]);
     let getCurrentBlockHeight = () => Js.Promise.resolve(1);
     let broadcastTransaction = broadcastTransaction(config);
   });
