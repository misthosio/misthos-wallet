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

module AccountKeyChainIndex: {include WalletType;};

type accountKeyChainIdx = AccountKeyChainIndex.t;

module CoSignerIndex: {include WalletType; let fromInt: int => t;};

type coSignerIdx = CoSignerIndex.t;

module ChainIndex: {
  include WalletType;
  let externalChain: t;
  let internalChain: t;
};

type chainIdx = ChainIndex.t;

module AddressIndex: {include WalletType; let fromInt: int => t;};

type addressIdx = AddressIndex.t;
