let satoshisPerBTC = 1e8;

type config = {
  bitcoindUrl: string,
  rpcUser: string,
  rpcPassword: string
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
        ()
      )
    )
    |> then_(Fetch.Response.json)
  );

let getBlockHeight = ({bitcoindUrl} as config) => {
  let jsonRPC =
    Json.Encode.(
      object_([
        ("jsonrpc", string("1.0")),
        ("method", string("getblockcount"))
      ])
    )
    |> Json.stringify;
  Js.Promise.(
    rpcCall(config, jsonRPC)
    |> then_(obj => resolve(Json.Decode.(field("result", int, obj))))
  );
};

type bitcoindUTXO = {
  txId: string,
  txOutputN: int,
  address: string,
  amount: float,
  satoshis: float,
  confirmations: int
};

let getUTXOs = ({bitcoindUrl} as config, address) => {
  let jsonRPCImport =
    Json.Encode.(
      object_([
        ("jsonrpc", string("1.0")),
        ("method", string("importaddress")),
        ("params", list(string, [address]))
      ])
    )
    |> Json.stringify;
  let jsonRPCUnspent =
    Json.Encode.(
      object_([
        ("jsonrpc", string("1.0")),
        ("method", string("listunspent")),
        ("params", tuple3(int, int, list(string), (1, 9999999, [address])))
      ])
    )
    |> Json.stringify;
  Js.Promise.(
    rpcCall(config, jsonRPCImport)
    |> then_(_response => rpcCall(config, jsonRPCUnspent))
    |> then_(obj =>
         Json.Decode.(
           obj
           |> field(
                "result",
                withDefault(
                  [],
                  list(utxo =>
                    {
                      txId: utxo |> field("txid", string),
                      txOutputN: utxo |> field("vout", int),
                      address: utxo |> field("address", string),
                      amount: utxo |> field("amount", float),
                      satoshis: field("amount", float, utxo) *. satoshisPerBTC,
                      confirmations: utxo |> field("confirmations", int)
                    }
                  )
                )
              )
         )
         |> resolve
       )
  );
};

let broadcastTransaction = (config, transaction) => {
  let jsonRPC =
    Json.Encode.(
      object_([
        ("jsonrpc", string("1.0")),
        ("method", string("sendrawtransaction")),
        ("params", list(string, [transaction |> Bitcoin.Tx.toHex]))
      ])
    )
    |> Json.stringify;
  Js.Promise.(rpcCall(config, jsonRPC));
};
