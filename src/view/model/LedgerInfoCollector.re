open Belt;

open PrimitiveTypes;

open WalletTypes;

open Event;

type t = {
  localUser: userId,
  ledgerIds: AccountIndex.map(string),
  ledgerUpToDate: AccountIndex.map(bool),
  nextKeyChainIdx: AccountIndex.map(CustodianKeyChainIndex.t),
  ledgerConnected: AccountIndex.map(UserId.set),
};

let make = localUser => {
  localUser,
  ledgerIds: AccountIndex.makeMap(),
  ledgerUpToDate: AccountIndex.makeMap(),
  nextKeyChainIdx: AccountIndex.makeMap(),
  ledgerConnected: AccountIndex.makeMap(),
};

let ledgerId = (accountIdx, {ledgerIds}) =>
  ledgerIds |. Map.get(accountIdx);
let ledgerUpToDate = (accountIdx, {ledgerUpToDate}) =>
  ledgerUpToDate |. Map.getWithDefault(accountIdx, false);
let nextKeyChainIdx = (accountIdx, {nextKeyChainIdx}) =>
  nextKeyChainIdx
  |. Map.getWithDefault(accountIdx, CustodianKeyChainIndex.first);
let ledgerConnected = (accountIdx, {ledgerConnected}) =>
  ledgerConnected |. Map.getWithDefault(accountIdx, UserId.emptySet);

let apply = (event, state) =>
  switch (event) {
  | CustodianKeyChainUpdated({custodianId, keyChain})
      when UserId.eq(custodianId, state.localUser) =>
    let accountIdx = keyChain |> CustodianKeyChain.accountIdx;
    {
      ...state,
      ledgerIds:
        switch (keyChain |> CustodianKeyChain.hardwareId) {
        | Some(id) => state.ledgerIds |. Map.set(accountIdx, id)
        | _ => state.ledgerIds
        },
      ledgerUpToDate: state.ledgerUpToDate |. Map.set(accountIdx, true),
      nextKeyChainIdx:
        state.nextKeyChainIdx
        |. Map.set(
             accountIdx,
             keyChain
             |> CustodianKeyChain.keyChainIdx
             |> CustodianKeyChainIndex.next,
           ),
      ledgerConnected:
        switch (keyChain |> CustodianKeyChain.hardwareId) {
        | Some(_) =>
          state.ledgerConnected
          |. Map.updateU(accountIdx, (. users) =>
               users
               |> Utils.mapOption(users => users |. Set.add(custodianId))
               |> Js.Option.getWithDefault(
                    UserId.emptySet |. Set.add(custodianId),
                  )
               |. Some
             )
        | _ => state.ledgerConnected
        },
    };
  | PartnerRemovalAccepted({data: {id}})
      when UserId.neq(id, state.localUser) => {
      ...state,
      ledgerUpToDate: AccountIndex.makeMap(),
    }
  | _ => state
  };
