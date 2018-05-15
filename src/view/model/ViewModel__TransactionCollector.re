open Belt;

open PrimitiveTypes;

type confirmedTx =
  | ConfirmedIncome(string, BTC.t, Js.Date.t)
  | ConfirmedPayout(string, BTC.t, Js.Date.t);

type unconfirmedTx =
  | UnconfirmedIncome(string, BTC.t)
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

let mapConfirmation =
    (
      {txId, unixTime}: Event.Transaction.Confirmed.t,
      {unconfirmedTxs, confirmedTxs} as state,
    ) => {
  let newTxs =
    unconfirmedTxs
    |. List.keepMap(
         fun
         | UnconfirmedIncome(incomeTx, amount) when incomeTx == txId =>
           Some(
             ConfirmedIncome(
               txId,
               amount,
               Js.Date.fromFloat(unixTime *. 1000.),
             ),
           )
         | UnconfirmedPayout(payoutTx, amount) when payoutTx == txId =>
           Some(
             ConfirmedPayout(
               txId,
               amount,
               Js.Date.fromFloat(unixTime *. 1000.),
             ),
           )
         | _ => None,
       );
  let newUnconf =
    unconfirmedTxs
    |. List.keep(
         fun
         | UnconfirmedIncome(incomeTx, _) when incomeTx == txId => false
         | UnconfirmedPayout(payoutTx, _) when payoutTx == txId => false
         | _ => true,
       );
  {
    ...state,
    unconfirmedTxs: newUnconf,
    confirmedTxs: newTxs |. List.concat(confirmedTxs),
  };
};

let apply = (event: Event.t, state) =>
  switch (event) {
  | VentureCreated({network}) => {...state, network}
  | IncomeDetected({txId, amount}) => {
      ...state,
      unconfirmedTxs: [
        UnconfirmedIncome(txId, amount),
        ...state.unconfirmedTxs,
      ],
    }
  | PayoutProposed({data: {payoutTx}, processId}) => {
      ...state,
      payoutProcesses: state.payoutProcesses |. Map.set(processId, payoutTx),
    }
  | PayoutBroadcast({txId, processId}) =>
    let payoutTx: PayoutTransaction.t =
      state.payoutProcesses |. Map.getExn(processId);
    {
      ...state,
      unconfirmedTxs: [
        UnconfirmedPayout(
          txId,
          PayoutTransaction.summary(state.network, payoutTx).spentWithFees,
        ),
        ...state.unconfirmedTxs,
      ],
    };
  | TransactionConfirmed(event) => state |> mapConfirmation(event)
  | _ => state
  };
