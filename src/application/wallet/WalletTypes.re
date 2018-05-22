type utxo = {
  txId: string,
  txOutputN: int,
  address: string,
  amount: BTC.t,
  confirmations: int,
};

type txInfo = {
  txId: string,
  blockHeight: option(float),
  unixTime: option(float),
};

type broadcastResult =
  | Ok(string)
  | AlreadyInBlockchain
  | Error(string)
  | FetchError(Js.Promise.error);

module type NetworkClient = {
  let network: Bitcoin.Networks.t;
  let getUTXOs: list(string) => Js.Promise.t(list(utxo));
  let getTransactionInfo: Belt.Set.String.t => Js.Promise.t(list(txInfo));
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
  let fromInt: int => t;
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

module CoSignerIndex = {
  include Base;
};

type coSignerIdx = CoSignerIndex.t;

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
