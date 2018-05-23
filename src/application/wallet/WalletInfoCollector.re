open Belt;

open PrimitiveTypes;

open WalletTypes;

open Event;

open Address;

type t = {
  network: Network.t,
  unused: Network.inputSet,
  reserved: Network.inputSet,
  keyChains: AccountKeyChain.Collection.t,
  payoutProcesses: ProcessId.map(PayoutTransaction.t),
  activatedKeyChain:
    list((accountIdx, list((userId, AccountKeyChain.Identifier.t)))),
  exposedCoordinates: list(Address.Coordinates.t),
};

let currentKeyChainIdent = (accountIdx, userId, {activatedKeyChain}) =>
  activatedKeyChain
  |. List.getAssoc(accountIdx, AccountIndex.eq)
  |> Js.Option.getExn
  |. List.getAssoc(userId, UserId.eq)
  |> Js.Option.getExn;

let oldInputs = (accountIdx, userId, {keyChains, unused} as collector) => {
  let keyChainIdent = currentKeyChainIdent(accountIdx, userId, collector);
  let currentKeyChain =
    keyChains |> AccountKeyChain.Collection.lookup(accountIdx, keyChainIdent);
  let custodians = currentKeyChain |> AccountKeyChain.custodians;
  let currentKeyChainIdents =
    keyChains |> AccountKeyChain.Collection.withCustodians(custodians);
  unused
  |. Belt.Set.keepU((. i: Network.txInput) =>
       i.coordinates
       |> Coordinates.keyChainIdent
       |> Set.String.has(currentKeyChainIdents) == false
     );
};

let nextChangeAddress = (accountIdx, userId, collector) => {
  let keyChainIdent = currentKeyChainIdent(accountIdx, userId, collector);
  let accountKeyChain =
    collector.keyChains
    |> AccountKeyChain.Collection.lookup(accountIdx, keyChainIdent);
  let coordinates =
    collector.exposedCoordinates |> Coordinates.allForAccount(accountIdx);
  let nextChangeCoordinates =
    Coordinates.nextInternal(userId, coordinates, accountKeyChain);
  Address.find(nextChangeCoordinates, collector.keyChains);
};

let make = () => {
  network: Regtest,
  unused: Network.inputSet(),
  reserved: Network.inputSet(),
  keyChains: AccountKeyChain.Collection.empty,
  payoutProcesses: ProcessId.makeMap(),
  activatedKeyChain: [],
  exposedCoordinates: [],
};

let apply = (event, state) =>
  switch (event) {
  | VentureCreated({network}) => {...state, network}
  | AccountCreationAccepted(
      ({data: {accountIdx}}: AccountCreation.Accepted.t),
    ) => {
      ...state,
      activatedKeyChain: [(accountIdx, []), ...state.activatedKeyChain],
    }
  | AccountKeyChainIdentified({keyChain}) => {
      ...state,
      keyChains: state.keyChains |> AccountKeyChain.Collection.add(keyChain),
    }
  | AccountKeyChainActivated({accountIdx, custodianId, identifier}) => {
      ...state,
      activatedKeyChain: [
        (
          accountIdx,
          [
            (custodianId, identifier),
            ...state.activatedKeyChain
               |. List.getAssoc(accountIdx, AccountIndex.eq)
               |> Js.Option.getExn,
          ],
        ),
        ...state.activatedKeyChain
           |. List.removeAssoc(accountIdx, AccountIndex.eq),
      ],
    }
  | IncomeAddressExposed(({address: {coordinates}}: IncomeAddressExposed.t)) => {
      ...state,
      exposedCoordinates: [coordinates, ...state.exposedCoordinates],
    }
  | IncomeDetected({address, txId, txOutputN, amount, coordinates}) =>
    let keyChain =
      state.keyChains
      |> AccountKeyChain.Collection.lookup(
           coordinates |> Address.Coordinates.accountIdx,
           coordinates |> Address.Coordinates.keyChainIdent,
         );
    {
      ...state,
      unused:
        state.unused
        |. Set.add({
             txId,
             txOutputN,
             address,
             value: amount,
             coordinates,
             nCoSigners: keyChain.nCoSigners,
             nPubKeys: keyChain.custodianKeyChains |> List.length,
           }),
    };
  | PayoutProposed({
      data: {payoutTx: {usedInputs, changeAddress} as payoutTx},
      processId,
    }) => {
      ...state,
      unused: state.unused |. Set.removeMany(usedInputs),
      reserved: state.reserved |. Set.mergeMany(usedInputs),
      payoutProcesses: state.payoutProcesses |. Map.set(processId, payoutTx),
      exposedCoordinates:
        switch (changeAddress) {
        | None => state.exposedCoordinates
        | Some(changeAddress) => [
            changeAddress.coordinates,
            ...state.exposedCoordinates,
          ]
        },
    }
  | PayoutDenied({processId}) =>
    let payoutTx: PayoutTransaction.t =
      state.payoutProcesses |. Map.getExn(processId);
    {
      ...state,
      unused: state.unused |. Set.mergeMany(payoutTx.usedInputs),
      reserved: state.reserved |. Set.removeMany(payoutTx.usedInputs),
    };
  | PayoutBroadcast({processId, txId}) =>
    let payoutTx: PayoutTransaction.t =
      state.payoutProcesses |. Map.getExn(processId);
    {
      ...state,
      reserved: state.reserved |. Set.removeMany(payoutTx.usedInputs),
      unused:
        switch (
          payoutTx
          |> PayoutTransaction.txInputForChangeAddress(~txId, state.network)
        ) {
        | Some(input) => state.unused |. Set.add(input)
        | None => state.unused
        },
    };
  | PayoutBroadcastFailed({processId}) =>
    let payoutTx: PayoutTransaction.t =
      state.payoutProcesses |. Map.getExn(processId);
    {
      ...state,
      unused: state.unused |. Set.mergeMany(payoutTx.usedInputs),
      reserved: state.reserved |. Set.removeMany(payoutTx.usedInputs),
    };
  | _ => state
  };
