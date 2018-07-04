open Belt;

open PrimitiveTypes;

open Event;

type t = {
  network: Network.t,
  ventureId,
  transactionsOfInterest: Set.String.t,
  knownIncomeTxs: Set.String.t,
  confirmedTransactions: Map.String.t(float),
  notYetBroadcastPayouts: ProcessId.map(Payout.Finalized.t),
};

let make = () => {
  network: Regtest,
  ventureId: VentureId.fromString(""),
  transactionsOfInterest: Set.String.empty,
  knownIncomeTxs: Set.String.empty,
  confirmedTransactions: Map.String.empty,
  notYetBroadcastPayouts: ProcessId.makeMap(),
};

let apply = (event, state) =>
  switch (event) {
  | VentureCreated({network, ventureId}) => {...state, network, ventureId}
  | IncomeDetected({txId}) => {
      ...state,
      transactionsOfInterest:
        state.transactionsOfInterest |. Set.String.add(txId),
      knownIncomeTxs: state.knownIncomeTxs |. Set.String.add(txId),
    }
  | PayoutFinalized({processId} as finalizedTx) => {
      ...state,
      notYetBroadcastPayouts:
        state.notYetBroadcastPayouts |. Map.set(processId, finalizedTx),
    }
  | PayoutBroadcast({processId, txId}) => {
      ...state,
      notYetBroadcastPayouts:
        state.notYetBroadcastPayouts |. Map.remove(processId),
      transactionsOfInterest:
        state.transactionsOfInterest |. Set.String.add(txId),
    }
  | PayoutBroadcastFailed({processId}) => {
      ...state,
      notYetBroadcastPayouts:
        state.notYetBroadcastPayouts |. Map.remove(processId),
    }
  | TransactionConfirmed({txId, blockHeight}) => {
      ...state,
      confirmedTransactions:
        state.confirmedTransactions |. Map.String.set(txId, blockHeight),
    }
  | _ => state
  };
