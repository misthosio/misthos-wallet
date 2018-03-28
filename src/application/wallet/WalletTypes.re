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

module AddressIndex = {
  include Base;
};

type addressIdx = AddressIndex.t;
