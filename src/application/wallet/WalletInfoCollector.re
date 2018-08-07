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
  | Income(userId)
  | Change;

type addressInfo = {
  addressType,
  custodians: UserId.set,
  address: string,
  nCoSigners: int,
  addressStatus,
  balance: BTC.t,
};

type t = {
  network: Network.t,
  spendable: AccountIndex.map(Map.String.t(list(Network.txInput))),
  oldSpendable: AccountIndex.map(Map.String.t(list(Network.txInput))),
  unlocked: AccountIndex.map(Network.inputSet),
  temporarilyInaccessible:
    AccountIndex.map(Map.String.t(list(Network.txInput))),
  inaccessible: AccountIndex.map(Map.String.t(list(Network.txInput))),
  reserved: AccountIndex.map(Network.inputMap(ProcessId.set)),
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

let collidingProcesses = (accountIdx, processId, {reserved, payoutProcesses}) =>
  payoutProcesses
  |. Map.get(processId)
  |> Utils.mapOption(({usedInputs}: PayoutTransaction.t) => usedInputs)
  |> Js.Option.getWithDefault([||])
  |. Array.reduceU(ProcessId.emptySet, (. res, input) =>
       reserved
       |. Map.getWithDefault(accountIdx, Network.inputMap())
       |. Map.getWithDefault(input, ProcessId.emptySet)
       |. Set.union(res)
     )
  |. Set.remove(processId);

let totalUnusedBTC = (accountIdx, {spendable, oldSpendable, unlocked}) => {
  let (usedInputs, result) =
    spendable
    |. Map.getWithDefault(accountIdx, Map.String.empty)
    |. Map.String.reduceU(
         (Network.inputSet(), BTC.zero),
         (. res, _, inputs: list(Network.txInput)) =>
         inputs
         |. List.reduceU(
              res, (. (inputSet, res), {value} as input: Network.txInput) =>
              (inputSet |. Set.add(input), res |> BTC.plus(value))
            )
       )
    |. Map.String.reduceU(
         oldSpendable |. Map.getWithDefault(accountIdx, Map.String.empty),
         _,
         (. res, _, inputs: list(Network.txInput)) =>
         inputs
         |. List.reduceU(
              res, (. (inputSet, res), {value} as input: Network.txInput) =>
              (inputSet |. Set.add(input), res |> BTC.plus(value))
            )
       );
  unlocked
  |. Map.getWithDefault(accountIdx, Network.inputSet())
  |. Set.diff(usedInputs)
  |. Set.reduceU(result, (. res, {value}: Network.txInput) =>
       res |> BTC.plus(value)
     );
};

let totalReservedBTC = (accountIdx, {reserved}) =>
  reserved
  |. Map.getWithDefault(accountIdx, Network.inputMap())
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

let inputsFor = (accountIdx, info, state) =>
  (
    switch (info.addressStatus) {
    | Accessible => state.spendable
    | AtRisk
    | OutdatedCustodians => state.oldSpendable
    | TemporarilyInaccessible => state.temporarilyInaccessible
    | Inaccessible => state.inaccessible
    }
  )
  |. Map.getWithDefault(accountIdx, Map.String.empty)
  |. Map.String.getWithDefault(info.address, []);

let currentSpendableInputs = (accountIdx, {reserved, spendable}) =>
  spendable
  |. Map.getWithDefault(accountIdx, Map.String.empty)
  |. Map.String.reduceU(Network.inputSet(), (. res, _, inputs) =>
       res |. Set.mergeMany(inputs |> List.toArray)
     )
  |. Set.diff(
       reserved
       |. Map.getWithDefault(accountIdx, Network.inputMap())
       |> Map.keysToArray
       |> Set.mergeMany(Network.inputSet()),
     );
let unlockedInputs = (accountIdx, {unlocked} as collector) =>
  unlocked
  |. Map.getWithDefault(accountIdx, Network.inputSet())
  |. Set.keepU((. {address}: Network.txInput) =>
       addressInfoFor(accountIdx, address, collector).addressStatus
       != Inaccessible
     );

let oldSpendableInputs = (accountIdx, {reserved, oldSpendable}) =>
  oldSpendable
  |. Map.getWithDefault(accountIdx, Map.String.empty)
  |. Map.String.reduceU(Network.inputSet(), (. res, _, inputs) =>
       res |. Set.mergeMany(inputs |> List.toArray)
     )
  |. Set.diff(
       reserved
       |. Map.getWithDefault(accountIdx, Network.inputMap())
       |> Map.keysToArray
       |> Set.mergeMany(Network.inputSet()),
     );
let allUnspentInputs = ({oldSpendable, spendable, temporarilyInaccessible}) =>
  temporarilyInaccessible
  |. Map.valuesToArray
  |. Array.reduceU(Network.inputSet(), (. res, map) =>
       map
       |. Map.String.reduceU(res, (. res, _, inputs) =>
            res |. Set.mergeMany(inputs |> List.toArray)
          )
     )
  |. Array.reduceU(oldSpendable |> Map.valuesToArray, _, (. res, map) =>
       map
       |. Map.String.reduceU(res, (. res, _, inputs) =>
            res |. Set.mergeMany(inputs |> List.toArray)
          )
     )
  |. Array.reduceU(spendable |> Map.valuesToArray, _, (. res, map) =>
       map
       |. Map.String.reduceU(res, (. res, _, inputs) =>
            res |. Set.mergeMany(inputs |> List.toArray)
          )
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
  spendable: AccountIndex.makeMap(),
  oldSpendable: AccountIndex.makeMap(),
  unlocked: AccountIndex.makeMap(),
  temporarilyInaccessible: AccountIndex.makeMap(),
  inaccessible: AccountIndex.makeMap(),
  reserved: AccountIndex.makeMap(),
  keyChains: AccountKeyChain.Collection.empty,
  payoutProcesses: ProcessId.makeMap(),
  activatedKeyChain: [],
  exposedCoordinates: [],
  addressInfos: AccountIndex.makeMap(),
  currentCustodians: AccountIndex.makeMap(),
};

let removeInputsFromReserved = (accountIdx, processId, inputs, reserved) =>
  reserved
  |. Map.updateU(
       accountIdx,
       (. reserved) => {
         let reserved =
           reserved |> Js.Option.getWithDefault(Network.inputMap());
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
            )
         |. Some;
       },
     );

let removeAddressFrom = (accountIdx, address, status, state) =>
  switch (status) {
  | Accessible =>
    let accountSpendable =
      state.spendable |. Map.getWithDefault(accountIdx, Map.String.empty);
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
      state.oldSpendable |. Map.getWithDefault(accountIdx, Map.String.empty);

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
      |. Map.getWithDefault(accountIdx, Map.String.empty);
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
      state.inaccessible |. Map.getWithDefault(accountIdx, Map.String.empty);
    let inputs = accountInaccessible |. Map.String.get(address);
    (
      inputs,
      {
        ...state,
        inaccessible:
          state.inaccessible
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
let moveTxInputs = (accountIdx, address, newStatus, oldStatus, state) => {
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
    } else if (nIntersect == nCoSigners) {
      AtRisk;
    } else {
      OutdatedCustodians;
    };
  };
let updateAddressInfos = (accountIdx, currentCustodians, state) => {
  let custodians =
    currentCustodians |. Map.getWithDefault(accountIdx, UserId.emptySet);
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

let addToBalance = (accountIdx, address, amount, state) => {
  ...state,
  addressInfos:
    state.addressInfos
    |. Map.updateU(accountIdx, (. infos) =>
         infos
         |> Js.Option.getWithDefault([])
         |. List.mapU((. info) =>
              info.address == address ?
                {...info, balance: info.balance |> BTC.plus(amount)} : info
            )
         |. Some
       ),
};
let removeInputFromUtxoMap = (accountIdx, input: Network.txInput, inputMap) =>
  inputMap
  |. Map.updateU(
       accountIdx,
       (. map) => {
         let map =
           map
           |> Js.Option.getWithDefault(Map.String.empty)
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
              );
         map |. Map.String.size > 0 ? Some(map) : None;
       },
     );
let removeInput = (accountIdx, input: Network.txInput, state) => {
  let info = state |> addressInfoFor(accountIdx, input.address);
  switch (info.addressStatus) {
  | Accessible => {
      ...state,
      spendable: state.spendable |> removeInputFromUtxoMap(accountIdx, input),
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
        {partnerId, address: {coordinates, displayAddress, nCoSigners}}: Income.AddressExposed.t
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
                   addressType: Income(partnerId),
                   nCoSigners,
                   custodians,
                   balance: BTC.zero,
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
      unlocked: false,
    };
    state
    |> addTxInput(addressStatus, accountIdx, input)
    |> addToBalance(accountIdx, address, amount);
  | IncomeUnlocked({input}) =>
    let accountIdx = input.coordinates |> Address.Coordinates.accountIdx;
    let addressStatus =
      (state |> addressInfoFor(accountIdx, input.address)).addressStatus;
    let updateInput = (input: Network.txInput, utxoMap) =>
      utxoMap
      |. Map.updateU(accountIdx, (. inputs) =>
           inputs
           |> Js.Option.getWithDefault(Map.String.empty)
           |. Map.String.updateU(input.address, (. inputs) =>
                inputs
                |> Js.Option.getWithDefault([])
                |. List.mapU((. in_) =>
                     Network.TxInputCmp.compareInputs(. in_, input) == 0 ?
                       input : in_
                   )
                |. Some
              )
           |. Some
         );
    {
      ...
        switch (addressStatus) {
        | Accessible => {
            ...state,
            spendable: state.spendable |> updateInput(input),
          }
        | AtRisk
        | OutdatedCustodians => {
            ...state,
            oldSpendable: state.oldSpendable |> updateInput(input),
          }
        | TemporarilyInaccessible => {
            ...state,
            temporarilyInaccessible:
              state.temporarilyInaccessible |> updateInput(input),
          }
        | Inaccessible => {
            ...state,
            inaccessible: state.inaccessible |> updateInput(input),
          }
        },
      unlocked:
        state.unlocked
        |. Map.updateU(accountIdx, (. inputs) =>
             inputs
             |> Js.Option.getWithDefault(Network.inputSet())
             |. Set.add(input)
             |. Some
           ),
    };
  | PayoutProposed({
      data: {payoutTx: {usedInputs, changeAddress} as payoutTx},
      processId,
    }) =>
    let accountIdx =
      (usedInputs |. Array.getExn(0)).coordinates
      |> Address.Coordinates.accountIdx;
    {
      ...state,
      reserved:
        state.reserved
        |. Map.updateU(
             accountIdx,
             (. reserved) => {
               let reserved =
                 reserved |> Js.Option.getWithDefault(Network.inputMap());
               usedInputs
               |. Array.reduceU(reserved, (. lookup, input) =>
                    lookup
                    |. Map.updateU(input, (. processes) =>
                         Some(
                           processes
                           |> Js.Option.getWithDefault(ProcessId.emptySet)
                           |. Set.add(processId),
                         )
                       )
                  )
               |. Some;
             },
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
    };
  | PayoutDenied({processId}) =>
    let payoutTx: PayoutTransaction.t =
      state.payoutProcesses |. Map.getExn(processId);
    let accountIdx =
      (payoutTx.usedInputs |. Array.getExn(0)).coordinates
      |> Address.Coordinates.accountIdx;
    let reserved =
      removeInputsFromReserved(
        accountIdx,
        processId,
        payoutTx.usedInputs,
        state.reserved,
      );
    {...state, reserved};
  | PayoutAborted({processId}) =>
    let payoutTx: PayoutTransaction.t =
      state.payoutProcesses |. Map.getExn(processId);
    let accountIdx =
      (payoutTx.usedInputs |. Array.getExn(0)).coordinates
      |> Address.Coordinates.accountIdx;
    let reserved =
      removeInputsFromReserved(
        accountIdx,
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
        accountIdx,
        processId,
        payoutTx.usedInputs,
        state.reserved,
      );
    let unlocked =
      state.unlocked
      |. Map.updateU(accountIdx, (. unlockedInputs) =>
           switch (unlockedInputs) {
           | Some(unlockedInputs) =>
             let unlockedInputs =
               payoutTx.usedInputs |. Array.reduce(unlockedInputs, Set.remove);
             unlockedInputs |. Set.size == 0 ? None : Some(unlockedInputs);
           | None => None
           }
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
        {
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
                       balance: BTC.zero,
                     },
                     ...infos,
                   ]);
                 },
               ),
        }
        |> addTxInput(addressStatus, accountIdx, changeInput)
        |> addToBalance(accountIdx, changeInput.address, changeInput.value);
      | None => state
      };
    {
      ...
        payoutTx.usedInputs
        |. Array.reduceU(state, (. state, input: Network.txInput) =>
             state
             |> removeInput(accountIdx, input)
             |> addToBalance(
                  accountIdx,
                  input.address,
                  input.value |. BTC.timesRounded(-1.),
                )
           ),
      reserved,
      unlocked,
    };
  | PayoutBroadcastFailed({processId}) =>
    let payoutTx: PayoutTransaction.t =
      state.payoutProcesses |. Map.getExn(processId);
    let accountIdx =
      (payoutTx.usedInputs |. Array.getExn(0)).coordinates
      |> Address.Coordinates.accountIdx;
    let reserved =
      removeInputsFromReserved(
        accountIdx,
        processId,
        payoutTx.usedInputs,
        state.reserved,
      );
    {...state, reserved};
  | _ => state
  };
