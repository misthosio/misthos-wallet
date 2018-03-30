type utxo = {
  txId: string,
  txOutputN: int,
  address: string,
  amount: BTC.t,
  confirmations: int
};

module type NetworkClient = {
  let network: Bitcoin.Networks.t;
  let getUTXOs: list(string) => Js.Promise.t(list(utxo));
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

module AddressIndex: {include WalletType;};

type addressIdx = AddressIndex.t;

module AddressCoordinates: {
  type t;
  let firstInternal: (accountIdx, accountKeyChainIdx) => t;
  let firstExternal: (accountIdx, accountKeyChainIdx) => t;
  let next: t => t;
  let lookupKeyChain:
    (t, list((accountIdx, list((accountKeyChainIdx, 'a))))) => 'a;
  let addressIdx: t => addressIdx;
  let chainIdx: t => int;
  let accountIdx: t => accountIdx;
  let encode: t => Js.Json.t;
  let decode: Js.Json.t => t;
};
