type utxo = {
  txId: string,
  txOutputN: int,
  address: string,
  amount: BTC.t,
};

module UtxoCmp = {
  let compareUtxos =
    (.
      {txId: id1, txOutputN: out1}: utxo,
      {txId: id2, txOutputN: out2}: utxo,
    ) => {
      let c = compare(id1, id2);
      if (c != 0) {
        c;
      } else {
        compare(out1, out2);
      };
    };
  include Belt.Id.MakeComparableU({
    type t = utxo;
    let cmp = compareUtxos;
  });
};
type utxoSet = Belt.Set.t(UtxoCmp.t, UtxoCmp.identity);
let emptyUtxoSet = Belt.Set.make(~id=(module UtxoCmp));

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

module type NetworkClientInterface = {
  let network: Bitcoin.Networks.t;
  let getUTXOs: list(string) => Js.Promise.t(utxoSet);
  let getTransactionInfo: Belt.Set.String.t => Js.Promise.t(list(txInfo));
  let getTransactionHex:
    array(string) => Js.Promise.t(array((string, string)));
  let getCurrentBlockHeight: unit => Js.Promise.t(int);
  let broadcastTransaction:
    Bitcoin.Transaction.t => Js.Promise.t(broadcastResult);
};

module Base = {
  type t = int;
  external toInt: int => 'a = "%identity";
  external fromInt: 'a => int = "%identity";
  let first = 0 |> toInt;
  let next = idx => toInt(idx) + 1 |> fromInt;
  let encode = id => toInt(id) |> Json.Encode.int;
  let decode = id => id |> Json.Decode.int |> fromInt;
  let compare = (a, b) => compare(toInt(a), toInt(b));
  let eq = (a, b) => compare(a, b) == 0;
  let neq = (a, b) => compare(a, b) != 0;
  module Comparator =
    Belt.Id.MakeComparableU({
      type nonrec t = t;
      let cmp = (. pA, pB) => compare(pA, pB);
    });
  type map('v) = Belt.Map.t(Comparator.t, 'v, Comparator.identity);
  let makeMap = () => Belt.Map.make(~id=(module Comparator));
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
  module Comparator: {
    type identity;
    type nonrec t = t;
    let cmp: Belt.Id.cmp(t, identity);
  };
  type map('v) = Belt.Map.t(t, 'v, Comparator.identity);
  let makeMap: unit => map('v);
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
