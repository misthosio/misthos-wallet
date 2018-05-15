open Belt;

open PrimitiveTypes;

type confirmedTx =
  | ConfirmedIncome(BTC.t, Js.Date.t)
  | ConfirmedPayout(BTC.t, Js.Date.t);

type unconfirmedTx =
  | UnconfirmedIncome(BTC.t)
  | UnconfirmedPayout(string, BTC.t);

type t = {
  payoutProcesses: ProcessId.map(PayoutTransaction.t),
  confirmedTxs: list(confirmedTx),
  unconfirmedTxs: list(unconfirmedTx),
  network: Network.t,
};

let make = () => {
  network: Regtest,
  confirmedTxs: [],
  unconfirmedTxs: [],
  payoutProcesses: ProcessId.makeMap(),
};

let apply = (event: Event.t, state) =>
  switch (event) {
  | VentureCreated({network}) => {...state, network}
  | IncomeDetected({amount, unixTime}) => {
      ...state,
      confirmedTxs: [
        ConfirmedIncome(amount, Js.Date.fromFloat(unixTime *. 1000.)),
        ...state.confirmedTxs,
      ],
    }
  | PayoutProposed({data: {payoutTx}, processId}) => {
      ...state,
      payoutProcesses: state.payoutProcesses |. Map.set(processId, payoutTx),
    }
  | PayoutBroadcast({transactionId, processId}) =>
    let payoutTx: PayoutTransaction.t =
      state.payoutProcesses |. Map.getExn(processId);
    {
      ...state,
      unconfirmedTxs: [
        UnconfirmedPayout(
          transactionId,
          PayoutTransaction.summary(state.network, payoutTx).spentWithFees,
        ),
        ...state.unconfirmedTxs,
      ],
    };
  | _ => state
  };
