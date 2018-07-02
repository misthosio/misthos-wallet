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
  addressStatus,
};

type t = {
  network: Network.t,
  unused: AccountIndex.map(Network.inputSet),
  spendable: AccountIndex.map(Map.String.t(list(Network.txInput))),
  oldSpendable: AccountIndex.map(Map.String.t(list(Network.txInput))),
  unlocked: AccountIndex.map(Network.inputSet),
  temporarilyInaccessible:
    AccountIndex.map(Map.String.t(list(Network.txInput))),
  inaccessible: AccountIndex.map(Map.String.t(list(Network.txInput))),
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

let addressInfoFor = (accountIdx, findAddress, collector) =>
  collector
  |> addressInfos(accountIdx)
  |. List.getByU((. {address}) => address == findAddress)
  |> Js.Option.getExn;

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

let currentSpendableInputs = (accountIdx, {reserved, spendable}) =>
  (
    switch (spendable |. Map.get(accountIdx)) {
    | None => Network.inputSet()
    | Some(map) =>
      map
      |. Map.String.reduceU(Network.inputSet(), (. res, _, inputs) =>
           res |. Set.mergeMany(inputs |> List.toArray)
         )
    }
  )
  |. Set.diff(
       reserved |> Map.keysToArray |> Set.mergeMany(Network.inputSet()),
     );

let unlockedInputs = (accountIdx, {unlocked}) =>
  unlocked |. Map.getExn(accountIdx);

let oldSpendableInputs = (accountIdx, {reserved, oldSpendable}) =>
  (
    switch (oldSpendable |. Map.get(accountIdx)) {
    | None => Network.inputSet()
    | Some(map) =>
      map
      |. Map.String.reduceU(Network.inputSet(), (. res, _, inputs) =>
           res |. Set.mergeMany(inputs |> List.toArray)
         )
    }
  )
  |. Set.diff(
       reserved |> Map.keysToArray |> Set.mergeMany(Network.inputSet()),
     );

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
  spendable: AccountIndex.makeMap(),
  oldSpendable: AccountIndex.makeMap(),
  unlocked: AccountIndex.makeMap(),
  temporarilyInaccessible: AccountIndex.makeMap(),
  inaccessible: AccountIndex.makeMap(),
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

let removeAddressFrom = (accountIdx, address, status, state) =>
  switch (status) {
  | Accessible =>
    let accountSpendable =
      state.spendable
      |. Map.get(accountIdx)
      |> Js.Option.getWithDefault(Map.String.empty);
    let inputs = accountSpendable |. Map.String.get(address);
    (
      inputs,
      {
        ...state,
        spendable:
          state.spendable
          |. Map.set(
               accountIdx,
               accountSpendable |. Map.String.remove(address),
             ),
      },
    );
  | AtRisk
  | OutdatedCustodians =>
    let accountOldSpendable =
      state.oldSpendable
      |. Map.get(accountIdx)
      |> Js.Option.getWithDefault(Map.String.empty);

    let inputs = accountOldSpendable |. Map.String.get(address);
    (
      inputs,
      {
        ...state,
        oldSpendable:
          state.oldSpendable
          |. Map.set(
               accountIdx,
               accountOldSpendable |. Map.String.remove(address),
             ),
      },
    );
  | TemporarilyInaccessible =>
    let accountTemporarilyInaccessible =
      state.temporarilyInaccessible
      |. Map.get(accountIdx)
      |> Js.Option.getWithDefault(Map.String.empty);
    let inputs = accountTemporarilyInaccessible |. Map.String.get(address);
    (
      inputs,
      {
        ...state,
        temporarilyInaccessible:
          state.temporarilyInaccessible
          |. Map.set(
               accountIdx,
               accountTemporarilyInaccessible |. Map.String.remove(address),
             ),
      },
    );
  | Inaccessible =>
    let accountInaccessible =
      state.temporarilyInaccessible
      |. Map.get(accountIdx)
      |> Js.Option.getWithDefault(Map.String.empty);
    let inputs = accountInaccessible |. Map.String.get(address);
    (
      inputs,
      {
        ...state,
        temporarilyInaccessible:
          state.temporarilyInaccessible
          |. Map.set(
               accountIdx,
               accountInaccessible |. Map.String.remove(address),
             ),
      },
    );
  };
let addInputsTo = (accountIdx, address, status, inputs, state) =>
  switch (status) {
  | Accessible => {
      ...state,
      spendable:
        state.spendable
        |. Map.updateU(
             accountIdx,
             (. map) => {
               let map = map |> Js.Option.getWithDefault(Map.String.empty);
               map |. Map.String.set(address, inputs) |. Some;
             },
           ),
    }
  | AtRisk
  | OutdatedCustodians => {
      ...state,
      oldSpendable:
        state.oldSpendable
        |. Map.updateU(
             accountIdx,
             (. map) => {
               let map = map |> Js.Option.getWithDefault(Map.String.empty);
               map |. Map.String.set(address, inputs) |. Some;
             },
           ),
    }
  | TemporarilyInaccessible => {
      ...state,
      temporarilyInaccessible:
        state.temporarilyInaccessible
        |. Map.updateU(
             accountIdx,
             (. map) => {
               let map = map |> Js.Option.getWithDefault(Map.String.empty);
               map |. Map.String.set(address, inputs) |. Some;
             },
           ),
    }
  | Inaccessible => {
      ...state,
      inaccessible:
        state.inaccessible
        |. Map.updateU(
             accountIdx,
             (. map) => {
               let map = map |> Js.Option.getWithDefault(Map.String.empty);
               map |. Map.String.set(address, inputs) |. Some;
             },
           ),
    }
  };
let moveTxInputs = (accountIdx, address, oldStatus, newStatus, state) => {
  let (inputs, state) =
    removeAddressFrom(accountIdx, address, oldStatus, state);
  switch (inputs) {
  | Some(inputs) =>
    addInputsTo(accountIdx, address, newStatus, inputs, state)
  | None => state
  };
};
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
let updateAddressInfos = (accountIdx, currentCustodians, state) => {
  let custodians =
    currentCustodians
    |. Map.get(accountIdx)
    |> Js.Option.getWithDefault(UserId.emptySet);
  let updates = ref([]);
  let state = {
    ...state,
    addressInfos:
      state.addressInfos
      |. Map.updateU(
           accountIdx,
           (. infos) => {
             let infos = infos |> Js.Option.getWithDefault([]);
             infos
             |. List.mapU((. info) => {
                  let newStatus =
                    determinAddressStatus(
                      custodians,
                      info.custodians,
                      info.nCoSigners,
                    );
                  if (newStatus != info.addressStatus) {
                    updates :=
                      [
                        (info.address, newStatus, info.addressStatus),
                        ...updates^,
                      ];
                  };
                  {...info, addressStatus: newStatus};
                })
             |. Some;
           },
         ),
  };
  updates^
  |. List.reduceU(state, (. state, (address, newStatus, oldStatus)) =>
       state |> moveTxInputs(accountIdx, address, newStatus, oldStatus)
     );
};

let addInputToUtxoMap = (accountIdx, input: Network.txInput, inputMap) =>
  inputMap
  |. Map.updateU(
       accountIdx,
       (. map) => {
         let map = map |> Js.Option.getWithDefault(Map.String.empty);
         map
         |. Map.String.updateU(
              input.address,
              (. set) => {
                let set = set |> Js.Option.getWithDefault([]);
                [input, ...set] |. Some;
              },
            )
         |. Some;
       },
     );
let addTxInput = (addressStatus, accountIdx, input, state) =>
  switch (addressStatus) {
  | Accessible => {
      ...state,
      spendable: state.spendable |> addInputToUtxoMap(accountIdx, input),
    }
  | AtRisk
  | OutdatedCustodians => {
      ...state,
      oldSpendable:
        state.oldSpendable |> addInputToUtxoMap(accountIdx, input),
    }
  | TemporarilyInaccessible => {
      ...state,
      temporarilyInaccessible:
        state.temporarilyInaccessible |> addInputToUtxoMap(accountIdx, input),
    }
  | Inaccessible => {
      ...state,
      inaccessible:
        state.inaccessible |> addInputToUtxoMap(accountIdx, input),
    }
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
    {...state, currentCustodians}
    |> updateAddressInfos(accountIdx, currentCustodians);
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
    {...state, currentCustodians}
    |> updateAddressInfos(accountIdx, currentCustodians);
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
    let addressStatus =
      (state |> addressInfoFor(accountIdx, address)).addressStatus;
    let keyChain =
      state.keyChains
      |> AccountKeyChain.Collection.lookup(
           accountIdx,
           coordinates |> Address.Coordinates.keyChainIdent,
         );
    let input: Network.txInput = {
      txId,
      txOutputN,
      address,
      value: amount,
      coordinates,
      nCoSigners: keyChain.nCoSigners,
      nPubKeys: keyChain.custodianKeyChains |> List.length,
      sequence: keyChain.sequence,
    };
    let state = addTxInput(addressStatus, accountIdx, input, state);
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
    let state =
      switch (
        payoutTx
        |> PayoutTransaction.txInputForChangeAddress(~txId, state.network)
      ) {
      | Some(changeInput) =>
        let custodians =
          (
            state.keyChains
            |> AccountKeyChain.Collection.lookup(
                 accountIdx,
                 changeInput.coordinates |> Address.Coordinates.keyChainIdent,
               )
          ).
            custodianKeyChains
          |. List.map(fst)
          |> List.toArray
          |> Set.mergeMany(UserId.emptySet);
        let addressStatus =
          determinAddressStatus(
            state.currentCustodians |. Map.getExn(accountIdx),
            custodians,
            changeInput.nCoSigners,
          );
        let state = {
          ...state,
          addressInfos:
            state.addressInfos
            |. Map.updateU(
                 accountIdx,
                 (. infos) => {
                   let infos = infos |> Js.Option.getWithDefault([]);
                   Some([
                     {
                       address: changeInput.address,
                       addressStatus,
                       addressType: Change,
                       nCoSigners: changeInput.nCoSigners,
                       custodians,
                     },
                     ...infos,
                   ]);
                 },
               ),
        };
        state |> addTxInput(addressStatus, accountIdx, changeInput);
      | None => state
      };
    let removeInputFromUtxoMap =
        (accountIdx, input: Network.txInput, inputMap) =>
      inputMap
      |. Map.updateU(
           accountIdx,
           (. map) => {
             let map = map |> Js.Option.getWithDefault(Map.String.empty);
             map
             |. Map.String.updateU(
                  input.address,
                  (. list_) => {
                    let list_ = list_ |> Js.Option.getWithDefault([]);
                    let res =
                      list_
                      |. List.keepU((. in_) =>
                           Network.TxInputCmp.compareInputs(. in_, input) != 0
                         );
                    res |. List.length > 0 ? Some(res) : None;
                  },
                )
             |. Some;
           },
         );
    let removeInput = (accountIdx, input: Network.txInput, state) => {
      let info = state |> addressInfoFor(accountIdx, input.address);
      switch (info.addressStatus) {
      | Accessible => {
          ...state,
          spendable:
            state.spendable |> removeInputFromUtxoMap(accountIdx, input),
        }
      | AtRisk
      | OutdatedCustodians => {
          ...state,
          oldSpendable:
            state.oldSpendable |> removeInputFromUtxoMap(accountIdx, input),
        }
      | TemporarilyInaccessible => {
          ...state,
          temporarilyInaccessible:
            state.temporarilyInaccessible
            |> removeInputFromUtxoMap(accountIdx, input),
        }
      | Inaccessible => {
          ...state,
          inaccessible:
            state.inaccessible |> removeInputFromUtxoMap(accountIdx, input),
        }
      };
    };
    let state =
      payoutTx.usedInputs
      |. Array.reduceU(state, (. state, input: Network.txInput) =>
           state |> removeInput(accountIdx, input)
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
