open Belt;

open PrimitiveTypes;

open WalletTypes;

open Event;

type t = {
  localUser: userId,
  ledgerId: option(string),
  ledgerUpToDate: bool,
  nextKeyChainIdx: AccountIndex.map(CustodianKeyChainIndex.t),
};

let make = localUser => {
  localUser,
  ledgerId: None,
  ledgerUpToDate: false,
  nextKeyChainIdx: AccountIndex.makeMap(),
};

let nextKeyChainIdx = (accountIdx, {nextKeyChainIdx}) =>
  nextKeyChainIdx
  |. Map.getWithDefault(accountIdx, CustodianKeyChainIndex.first);

let apply = (event, state) =>
  switch (event) {
  | _ => state
  };
