open Belt;

open PrimitiveTypes;

open WalletTypes;

open Event;

open Address;

type t = {
  network: Network.t,
  unused: Network.inputSet,
  reserved: Network.inputMap(ProcessId.set),
  keyChains: AccountKeyChain.Collection.t,
  payoutProcesses: ProcessId.map(PayoutTransaction.t),
  activatedKeyChain:
    list((accountIdx, list((userId, AccountKeyChain.Identifier.t)))),
  exposedCoordinates: list(Address.Coordinates.t),
};

let inputs =
  Inputs.inputs
  |> Json.parseOrRaise
  |> Json.Decode.array(Network.decodeInput)
  |> Set.mergeMany(Network.inputSet());

let nonReservedOldInputs = ({unused}) => (
  unused
  |. Belt.Set.keepU((. i: Network.txInput) =>
       if (i.txId
           == "35815aaadec8a110391de8ae2e8c304e3e6084d3cd1344d8155a2293ee54324b"
           ||
           i.txId == "d029a186f3d3124aca7fdc95d085ce25e0519918bf63ecb32cdfbb1da3268d8c") {
         false;
       } else {
         true;
       }
     ),
  inputs
  |. Belt.Set.keepU((. i: Network.txInput) =>
       if (i.txId
           == "35815aaadec8a110391de8ae2e8c304e3e6084d3cd1344d8155a2293ee54324b"
           ||
           i.txId == "d029a186f3d3124aca7fdc95d085ce25e0519918bf63ecb32cdfbb1da3268d8c") {
         false;
       } else {
         true;
       }
     ),
);

let make = () => {
  network: Regtest,
  unused: Network.inputSet(),
  reserved: Network.inputMap(),
  keyChains: AccountKeyChain.Collection.empty,
  payoutProcesses: ProcessId.makeMap(),
  activatedKeyChain: [],
  exposedCoordinates: [],
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
        (
          switch (
            payoutTx
            |> PayoutTransaction.txInputForChangeAddress(~txId, state.network)
          ) {
          | Some(input) => state.unused |. Set.add(input)
          | None => state.unused
          }
        )
        |. Set.removeMany(payoutTx.usedInputs),
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
