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

module AddressCoordinates = {
  type chainIdx = int;
  let externalChainIdx = 0;
  let internalChainIdx = 1;
  type t = (AccountIndex.t, AccountKeyChainIndex.t, chainIdx, AddressIndex.t);
  let firstExternal = (accountIdx, accountKeyChainIdx) => (
    accountIdx,
    accountKeyChainIdx,
    externalChainIdx,
    AccountIndex.first
  );
  let firstInternal = (accountIdx, accountKeyChainIdx) => (
    accountIdx,
    accountKeyChainIdx,
    internalChainIdx,
    AccountIndex.first
  );
  let next = ((accountIdx, accountKeyChainIdx, chainIdx, addressIdx)) => (
    accountIdx,
    accountKeyChainIdx,
    chainIdx,
    addressIdx |> AccountIndex.next
  );
  let lookupKeyChain =
      (
        (accountIdx, accountKeyChainIdx, _chainIdx, _addressIdx),
        accounts: list((accountIdx, list((accountKeyChainIdx, 'a))))
      ) =>
    accounts |> List.assoc(accountIdx) |> List.assoc(accountKeyChainIdx);
  let addressIdx = ((_, _, _, addressIdx)) => addressIdx;
  let chainIdx = ((_, _, chainIdx, _)) => chainIdx;
  let accountIdx = ((idx, _, _, _)) => idx;
  let encode =
    Json.Encode.(
      tuple4(
        AccountIndex.encode,
        AccountKeyChainIndex.encode,
        int,
        AddressIndex.encode
      )
    );
  let decode =
    Json.Decode.(
      tuple4(
        AccountIndex.decode,
        AccountKeyChainIndex.decode,
        int,
        AddressIndex.decode
      )
    );
};
