open Belt;

open PrimitiveTypes;

open WalletTypes;

open Event;

open Address;

type t = {
  network: Network.t,
  unused: Network.inputSet,
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

let nonReservedOldInputs = ({unused}) => {
  Js.log2("eq before?", Set.eq(unused, inputs));
  (
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
};

let make = () => {
  network: Regtest,
  unused: Network.inputSet(),
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
  | IncomeDetected({address, txId, txOutputN, amount}) => {
      ...state,
      unused:
        state.unused
        |. Set.add({
             txId,
             txOutputN,
             address,
             value: amount,
             nCoSigners: 2,
             nPubKeys: 3,
           }),
    }
  | PayoutProposed({data: {payoutTx}, processId}) => {
      ...state,
      payoutProcesses: state.payoutProcesses |. Map.set(processId, payoutTx),
    }
  | PayoutBroadcast({processId, txId}) =>
    let payoutTx: PayoutTransaction.t =
      state.payoutProcesses |. Map.getExn(processId);
    {
      ...state,
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
  | _ => state
  };
