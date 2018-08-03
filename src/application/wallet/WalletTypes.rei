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

module type NetworkClientInterface = {
  let network: Bitcoin.Networks.t;
  let getUTXOs: list(string) => Js.Promise.t(list(utxo));
  let getTransactionInfo: Belt.Set.String.t => Js.Promise.t(list(txInfo));
  let getTransactionHex: array(string) => Js.Promise.t(array(string));
  let getCurrentBlockHeight: unit => Js.Promise.t(int);
  let broadcastTransaction:
    Bitcoin.Transaction.t => Js.Promise.t(broadcastResult);
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

module AccountIndex: {include WalletType; let default: t;};

type accountIdx = AccountIndex.t;

module CustodianKeyChainIndex: {include WalletType; let first: t;};

type custodianKeyChainIdx = CustodianKeyChainIndex.t;

module AccountKeyChainIndex: {include WalletType;};

type accountKeyChainIdx = AccountKeyChainIndex.t;

module CoSignerIndex: {include WalletType;};

type coSignerIdx = CoSignerIndex.t;

module ChainIndex: {
  include WalletType;
  let externalChain: t;
  let internalChain: t;
};

type chainIdx = ChainIndex.t;

module AddressIndex: {include WalletType;};

type addressIdx = AddressIndex.t;
