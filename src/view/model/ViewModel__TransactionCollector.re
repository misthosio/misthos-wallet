open Belt;

open PrimitiveTypes;

type txType =
  | Income
  | Payout;

type txStatus =
  | Confirmed
  | Unconfirmed;

type txData = {
  txType,
  status: txStatus,
  txId: string,
  amount: BTC.t,
  date: option(Js.Date.t),
  detailsLink: Router.Config.route,
};

type t = {
  ventureId,
  payoutProcesses: ProcessId.map(PayoutTransaction.t),
  unconfirmedTxs: list(txData),
  confirmedTxs: list(txData),
  network: Network.t,
  txDates: Map.String.t(Js.Date.t),
};

let make = () => {
  ventureId: VentureId.fromString(""),
  network: Regtest,
  confirmedTxs: [],
  unconfirmedTxs: [],
  payoutProcesses: ProcessId.makeMap(),
  txDates: Map.String.empty,
};

let mapConfirmation =
    (
      {txId, unixTime}: Event.Transaction.Confirmed.t,
      {unconfirmedTxs, confirmedTxs} as state,
    ) => {
  let txDate = Js.Date.fromFloat(unixTime *. 1000.);
  let newTxs =
    unconfirmedTxs
    |. List.keepMap(({txId: dataId} as data) =>
         if (dataId == txId) {
           Some({...data, date: Some(txDate), status: Confirmed});
         } else {
           None;
         }
       );
  let newUnconf =
    unconfirmedTxs |. List.keep(({txId: dataId}) => dataId != txId);
  {
    ...state,
    txDates: state.txDates |. Map.String.set(txId, txDate),
    unconfirmedTxs: newUnconf,
    confirmedTxs: newTxs |. List.concat(confirmedTxs),
  };
};

let apply = (event: Event.t, state) =>
  switch (event) {
  | VentureCreated({network, ventureId}) => {...state, network, ventureId}
  | IncomeDetected({txId, amount}) => {
      ...state,
      unconfirmedTxs: [
        {
          txId,
          txType: Income,
          status: Unconfirmed,
          amount,
          date: None,
          detailsLink: Venture(state.ventureId, None),
        },
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
    let txDate = state.txDates |. Map.String.get(txId);
    let payout = {
      txId,
      txType: Payout,
      status: txDate |> Js.Option.isSome ? Confirmed : Unconfirmed,
      amount:
        PayoutTransaction.summary(state.network, payoutTx).spentWithFees,
      date: txDate,
      detailsLink: Venture(state.ventureId, Payout(processId)),
    };
    switch (payout.status) {
    | Confirmed => {...state, confirmedTxs: [payout, ...state.confirmedTxs]}
    | Unconfirmed => {
        ...state,
        unconfirmedTxs: [payout, ...state.unconfirmedTxs],
      }
    };
  | TransactionConfirmed(event) => state |> mapConfirmation(event)
  | _ => state
  };
