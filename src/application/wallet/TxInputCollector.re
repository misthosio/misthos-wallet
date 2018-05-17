open Belt;

open PrimitiveTypes;

open Event;

type t = {
  network: Network.t,
  unused: Network.inputSet,
  reserved: Network.inputSet,
  keyChains: AccountKeyChain.Collection.t,
  payoutProcesses: ProcessId.map(PayoutTransaction.t),
};

let make = () => {
  network: Regtest,
  unused: Network.inputSet(),
  reserved: Network.inputSet(),
  keyChains: AccountKeyChain.Collection.empty,
  payoutProcesses: ProcessId.makeMap(),
};

let apply = (event, state) =>
  switch (event) {
  | VentureCreated({network}) => {...state, network}
  | AccountKeyChainIdentified({keyChain}) => {
      ...state,
      keyChains: state.keyChains |> AccountKeyChain.Collection.add(keyChain),
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
  | PayoutProposed({data: {payoutTx}, processId}) => {
      ...state,
      unused: state.unused |. Set.removeMany(payoutTx.usedInputs),
      reserved: state.reserved |. Set.mergeMany(payoutTx.usedInputs),
      payoutProcesses: state.payoutProcesses |. Map.set(processId, payoutTx),
    }
  | PayoutBroadcast({processId, txId}) =>
    let payoutTx: PayoutTransaction.t =
      state.payoutProcesses |. Map.getExn(processId);
    {
      ...state,
      reserved: state.reserved |. Set.removeMany(payoutTx.usedInputs),
      unused:
        switch (
          payoutTx
          |> PayoutTransaction.txInputForChangeAddress(
               ~txId,
               state.keyChains,
               state.network,
             )
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
