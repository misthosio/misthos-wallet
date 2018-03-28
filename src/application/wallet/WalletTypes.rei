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
