type utxo = {
  txId: string,
  txOutputN: int,
  address: string,
  amount: BTC.t,
  confirmations: int
};

type broadcastResult =
  | Ok(string)
  | Error(string);

module type NetworkClient = {
  let network: Bitcoin.Networks.t;
  let getUTXOs: list(string) => Js.Promise.t(list(utxo));
  let broadcastTransaction:
    Bitcoin.Transaction.t => Js.Promise.t(broadcastResult);
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

module AccountIndex: {include WalletType; let default: t;};

type accountIdx = AccountIndex.t;

module CustodianKeyChainIndex: {include WalletType; let first: t;};

type custodianKeyChainIdx = CustodianKeyChainIndex.t;

module ChainIndex: {
  include WalletType;
  let externalChain: t;
  let internalChain: t;
};

type chainIdx = ChainIndex.t;

module AccountKeyChainIndex: {include WalletType;};

type accountKeyChainIdx = AccountKeyChainIndex.t;

module AddressIndex: {include WalletType;};

type addressIdx = AddressIndex.t;
