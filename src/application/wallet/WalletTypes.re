type utxo = {
  txId: string,
  txOutputN: int,
  address: string,
  amount: BTC.t,
  confirmations: int,
};

type output = {
  address: string,
  amount: BTC.t,
};

type transaction = {
  txId: string,
  outputs: list(output),
};

let encodeTransaction = transaction =>
  Json.Encode.(
    object_([
      ("txId", string(transaction.txId)),
      (
        "outputs",
        array(
          output =>
            object_([
              ("address", string(output.address)),
              ("amount", BTC.encode(output.amount)),
            ]),
          transaction.outputs |> Array.of_list,
        ),
      ),
    ])
  );

let decodeTransaction = raw =>
  Json.Decode.{
    txId: raw |> field("txid", string),
    outputs:
      raw
      |> field(
           "outputs",
           array(output =>
             {
               address: output |> field("address", string),
               amount: output |> field("value", string) |> BTC.fromString,
             }
           ),
         )
      |> Array.to_list,
  };

type broadcastResult =
  | Ok(string)
  | AlreadyInBlockchain
  | Error(string);

module type NetworkClient = {
  let network: Bitcoin.Networks.t;
  let getUTXOs: list(string) => Js.Promise.t(list(utxo));
  let broadcastTransaction:
    Bitcoin.Transaction.t => Js.Promise.t(broadcastResult);
};

module Base = {
  type t = int;
  external toInt : int => 'a = "%identity";
  external fromInt : 'a => int = "%identity";
  let first = 0 |> toInt;
  let next = idx => toInt(idx) + 1 |> fromInt;
  let encode = id => toInt(id) |> Json.Encode.int;
  let decode = id => id |> Json.Decode.int |> fromInt;
  let compare = (a, b) => compare(toInt(a), toInt(b));
  let eq = (a, b) => compare(a, b) == 0;
  let neq = (a, b) => compare(a, b) != 0;
};

module type WalletType = {
  type t;
  let toInt: t => int;
  let first: t;
  let next: t => t;
  let encode: t => Js.Json.t;
  let decode: Js.Json.t => t;
  let compare: (t, t) => int;
  let eq: (t, t) => bool;
  let neq: (t, t) => bool;
};

module AccountIndex = {
  include Base;
  let default = fromInt(0);
};

type accountIdx = AccountIndex.t;

module CustodianKeyChainIndex = {
  include Base;
  let first = fromInt(0);
};

type custodianKeyChainIdx = CustodianKeyChainIndex.t;

module AccountKeyChainIndex = {
  include Base;
};

type accountKeyChainIdx = AccountKeyChainIndex.t;

module ChainIndex = {
  include Base;
  let externalChain = fromInt(0);
  let internalChain = fromInt(1);
};

type chainIdx = ChainIndex.t;

module AddressIndex = {
  include Base;
};

type addressIdx = AddressIndex.t;
