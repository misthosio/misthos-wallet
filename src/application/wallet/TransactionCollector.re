open Belt;

open Event;

type t = {
  network: Network.t,
  transactionsOfInterest: Set.String.t,
  confirmedTransactions: Set.String.t,
};

let make = () => {
  network: Regtest,
  transactionsOfInterest: Set.String.empty,
  confirmedTransactions: Set.String.empty,
};

let apply = (event, state) =>
  switch (event) {
  | IncomeDetected({txId}) => {
      ...state,
      transactionsOfInterest:
        state.transactionsOfInterest |. Set.String.add(txId),
    }
  | PayoutBroadcast({txId}) => {
      ...state,
      transactionsOfInterest:
        state.transactionsOfInterest |. Set.String.add(txId),
    }
  | TransactionConfirmed({txId}) => {
      ...state,
      confirmedTransactions:
        state.confirmedTransactions |. Set.String.add(txId),
    }
  | _ => state
  };
