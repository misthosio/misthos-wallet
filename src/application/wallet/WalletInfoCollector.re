open Belt;

open PrimitiveTypes;

open WalletTypes;

open Event;

open Address;

type addressStatus =
  | Accessible
  | AtRisk
  | OutdatedCustodians
  | TemporarilyInaccessible
  | Inaccessible;

type addressType =
  | Income
  | Change;

type addressInfo = {
  addressType,
  custodians: UserId.set,
  address: string,
  nCoSigners: int,
  balance: BTC.t,
  addressStatus,
};

type t = {
  network: Network.t,
  unused: AccountIndex.map(Network.inputSet),
  reserved: Network.inputMap(ProcessId.set),
  keyChains: AccountKeyChain.Collection.t,
  payoutProcesses: ProcessId.map(PayoutTransaction.t),
  activatedKeyChain:
    list((accountIdx, list((userId, AccountKeyChain.Identifier.t)))),
  exposedCoordinates: list(Address.Coordinates.t),
  addressInfos: AccountIndex.map(list(addressInfo)),
  currentCustodians: AccountIndex.map(UserId.set),
};

let addressInfos = (accountIdx, {addressInfos}) =>
  addressInfos |. Map.get(accountIdx) |> Js.Option.getWithDefault([]);

let collidingProcesses = (processId, {reserved, payoutProcesses}) => {
  let inputs =
    payoutProcesses
    |. Map.get(processId)
    |> Utils.mapOption(({usedInputs}: PayoutTransaction.t) => usedInputs)
    |> Js.Option.getWithDefault([||]);
  inputs
  |. Array.reduceU(ProcessId.emptySet, (. res, input) =>
       reserved
       |. Map.getWithDefault(input, ProcessId.emptySet)
       |. Set.union(res)
     )
  |. Set.remove(processId);
};

let totalUnusedBTC = (accountIdx, {unused}) =>
  unused
  |. Map.getExn(accountIdx)
  |. Set.reduceU(BTC.zero, (. res, {value}: Network.txInput) =>
       res |> BTC.plus(value)
     );

let totalReservedBTC = ({reserved}) =>
  reserved
  |. Map.keysToArray
  |. Array.reduceU(BTC.zero, (. res, {value}: Network.txInput) =>
       res |> BTC.plus(value)
     );

let currentKeyChainIdent = (accountIdx, userId, {activatedKeyChain}) =>
  activatedKeyChain
  |. List.getAssoc(accountIdx, AccountIndex.eq)
  |> Js.Option.getExn
  |. List.getAssoc(userId, UserId.eq)
  |> Js.Option.getExn;

let currentKeyChain = (accountIdx, userId, {keyChains} as state) => {
  let currentIdent = currentKeyChainIdent(accountIdx, userId, state);
  keyChains |> AccountKeyChain.Collection.lookup(accountIdx, currentIdent);
};

let exposedCoordinates = ({exposedCoordinates}) => exposedCoordinates;

let accountKeyChains = ({keyChains}) => keyChains;

let unusedInputs = (accountIdx, {unused, reserved}) =>
  Set.diff(
    unused |. Map.getExn(accountIdx),
    reserved |> Map.keysToArray |> Set.mergeMany(Network.inputSet()),
  );

let nonReservedOldInputs = (accountIdx, userId, {keyChains} as collector) => {
  let keyChainIdent = currentKeyChainIdent(accountIdx, userId, collector);
  let currentKeyChain =
    keyChains |> AccountKeyChain.Collection.lookup(accountIdx, keyChainIdent);
  let custodians = currentKeyChain |> AccountKeyChain.custodians;
  let currentKeyChainIdents =
    keyChains |> AccountKeyChain.Collection.withCustodians(custodians);
  collector
  |> unusedInputs(accountIdx)
  |. Belt.Set.keepU((. i: Network.txInput) =>
       i.coordinates
       |> Coordinates.keyChainIdent
       |> Set.String.has(currentKeyChainIdents) == false
     );
};

let network = ({network}) => network;

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

let fakeChangeAddress = (accountIdx, userId, collector) => {
  let keyChainIdent = currentKeyChainIdent(accountIdx, userId, collector);
  let accountKeyChain =
    collector.keyChains
    |> AccountKeyChain.Collection.lookup(accountIdx, keyChainIdent);
  let coordinates =
    collector.exposedCoordinates |> Coordinates.allForAccount(accountIdx);
  let nextChangeCoordinates =
    Coordinates.nextInternal(userId, coordinates, accountKeyChain);
  {
    nCoSigners: accountKeyChain.nCoSigners,
    nPubKeys: accountKeyChain.custodianKeyChains |> List.length,
    coordinates: nextChangeCoordinates,
    witnessScript: "",
    redeemScript: "",
    displayAddress: Network.exampleOfLongestAddress(collector.network),
    sequence: accountKeyChain.nCoSigners > 1 ? Some(1) : None,
  };
};

let make = () => {
  network: Regtest,
  unused: AccountIndex.makeMap(),
  reserved: Network.inputMap(),
  keyChains: AccountKeyChain.Collection.empty,
  payoutProcesses: ProcessId.makeMap(),
  activatedKeyChain: [],
  exposedCoordinates: [],
  addressInfos: AccountIndex.makeMap(),
  currentCustodians: AccountIndex.makeMap(),
};

let removeInputsFromReserved = (processId, inputs, reserved) =>
  inputs
  |. Array.reduceU(reserved, (. lookup, input) =>
       lookup
       |. Map.updateU(
            input,
            (. processes) => {
              let processes =
                processes
                |> Js.Option.getWithDefault(ProcessId.emptySet)
                |. Set.remove(processId);
              processes |. Set.isEmpty ? None : Some(processes);
            },
          )
     );

let determinAddressStatus = (currentCustodians, addressCustodians, nCoSigners) =>
  if (currentCustodians |> Set.eq(addressCustodians)) {
    Accessible;
  } else {
    let intersection = addressCustodians |> Set.intersect(currentCustodians);
    let nIntersect = intersection |> Set.size;
    if (nIntersect == 0) {
      Inaccessible;
    } else if (nIntersect < nCoSigners) {
      TemporarilyInaccessible;
    } else if (intersection |> Set.eq(addressCustodians)) {
      if (addressCustodians
          |> Set.size == 1
          && currentCustodians
          |> Set.size > 1) {
        AtRisk;
      } else {
        OutdatedCustodians;
      };
    } else {
      AtRisk;
    };
  };
let updateAddressInfos = (accountIdx, currentCustodians, infos) => {
  let custodians =
    currentCustodians
    |. Map.get(accountIdx)
    |> Js.Option.getWithDefault(UserId.emptySet);

  infos
  |. Map.updateU(
       accountIdx,
       (. infos) => {
         let infos = infos |> Js.Option.getWithDefault([]);
         infos
         |. List.mapU((. info) =>
              {
                ...info,
                addressStatus:
                  determinAddressStatus(
                    custodians,
                    info.custodians,
                    info.nCoSigners,
                  ),
              }
            )
         |. Some;
       },
     );
};

let apply = (event, state) =>
  switch (event) {
  | VentureCreated({network}) => {...state, network}
  | CustodianAccepted({data: {accountIdx, partnerId}}) =>
    let currentCustodians =
      state.currentCustodians
      |. Map.updateU(accountIdx, (. custodians) =>
           Some(
             custodians
             |> Js.Option.getWithDefault(UserId.emptySet)
             |. Set.add(partnerId),
           )
         );
    {
      ...state,
      currentCustodians,
      addressInfos:
        updateAddressInfos(accountIdx, currentCustodians, state.addressInfos),
    };
  | CustodianRemovalAccepted({data: {accountIdx, custodianId}}) =>
    let currentCustodians =
      state.currentCustodians
      |. Map.updateU(accountIdx, (. custodians) =>
           Some(
             custodians
             |> Js.Option.getWithDefault(UserId.emptySet)
             |. Set.remove(custodianId),
           )
         );
    {
      ...state,
      currentCustodians,
      addressInfos:
        updateAddressInfos(accountIdx, currentCustodians, state.addressInfos),
    };
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
  | IncomeAddressExposed(
      (
        {address: {coordinates, displayAddress, nCoSigners}}: IncomeAddressExposed.t
      ),
    ) =>
    let accountIdx = coordinates |> Address.Coordinates.accountIdx;
    let custodians =
      (
        state.keyChains
        |> AccountKeyChain.Collection.lookup(
             accountIdx,
             coordinates |> Address.Coordinates.keyChainIdent,
           )
      ).
        custodianKeyChains
      |. List.map(fst)
      |> List.toArray
      |> Set.mergeMany(UserId.emptySet);
    {
      ...state,
      exposedCoordinates: [coordinates, ...state.exposedCoordinates],
      addressInfos:
        state.addressInfos
        |. Map.updateU(
             accountIdx,
             (. infos) => {
               let infos = infos |> Js.Option.getWithDefault([]);
               Some([
                 {
                   address: displayAddress,
                   addressStatus:
                     determinAddressStatus(
                       state.currentCustodians |. Map.getExn(accountIdx),
                       custodians,
                       nCoSigners,
                     ),
                   addressType: Income,
                   balance: BTC.zero,
                   nCoSigners,
                   custodians,
                 },
                 ...infos,
               ]);
             },
           ),
    };
  | IncomeDetected({address, txId, txOutputN, amount, coordinates}) =>
    let accountIdx = coordinates |> Address.Coordinates.accountIdx;
    let keyChain =
      state.keyChains
      |> AccountKeyChain.Collection.lookup(
           accountIdx,
           coordinates |> Address.Coordinates.keyChainIdent,
         );
    {
      ...state,
      unused:
        state.unused
        |. Map.updateU(
             accountIdx,
             (. unused) => {
               let unused =
                 unused |> Js.Option.getWithDefault(Network.inputSet());
               Some(
                 unused
                 |. Set.add({
                      txId,
                      txOutputN,
                      address,
                      value: amount,
                      coordinates,
                      nCoSigners: keyChain.nCoSigners,
                      nPubKeys: keyChain.custodianKeyChains |> List.length,
                      sequence: keyChain.sequence,
                    }),
               );
             },
           ),
    };
  | PayoutProposed({
      data: {payoutTx: {usedInputs, changeAddress} as payoutTx},
      processId,
    }) => {
      ...state,
      reserved:
        usedInputs
        |. Array.reduceU(state.reserved, (. lookup, input) =>
             lookup
             |. Map.updateU(input, (. processes) =>
                  Some(
                    processes
                    |> Js.Option.getWithDefault(ProcessId.emptySet)
                    |. Set.add(processId),
                  )
                )
           ),
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
    let reserved =
      removeInputsFromReserved(
        processId,
        payoutTx.usedInputs,
        state.reserved,
      );
    {...state, reserved};
  | PayoutAborted({processId}) =>
    let payoutTx: PayoutTransaction.t =
      state.payoutProcesses |. Map.getExn(processId);
    let reserved =
      removeInputsFromReserved(
        processId,
        payoutTx.usedInputs,
        state.reserved,
      );
    {...state, reserved};
  | PayoutBroadcast({processId, txId}) =>
    let payoutTx: PayoutTransaction.t =
      state.payoutProcesses |. Map.getExn(processId);
    let accountIdx =
      (payoutTx.usedInputs |. Array.getExn(0)).coordinates
      |> Address.Coordinates.accountIdx;
    let reserved =
      removeInputsFromReserved(
        processId,
        payoutTx.usedInputs,
        state.reserved,
      );
    {
      ...state,
      reserved,
      unused:
        state.unused
        |. Map.updateU(
             accountIdx,
             (. unused) => {
               let unused =
                 unused |> Js.Option.getWithDefault(Network.inputSet());
               (
                 switch (
                   payoutTx
                   |> PayoutTransaction.txInputForChangeAddress(
                        ~txId,
                        state.network,
                      )
                 ) {
                 | Some(input) => unused |. Set.add(input)
                 | None => unused
                 }
               )
               |. Set.removeMany(payoutTx.usedInputs)
               |. Some;
             },
           ),
    };
  | PayoutBroadcastFailed({processId}) =>
    let payoutTx: PayoutTransaction.t =
      state.payoutProcesses |. Map.getExn(processId);
    let reserved =
      removeInputsFromReserved(
        processId,
        payoutTx.usedInputs,
        state.reserved,
      );
    {...state, reserved};
  | _ => state
  };
