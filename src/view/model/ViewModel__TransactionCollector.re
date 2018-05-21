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
  confirmedTxs: list(txData),
  unconfirmedTxs: list(txData),
  network: Network.t,
};

let make = () => {
  ventureId: VentureId.fromString(""),
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
    |. List.keepMap(({txId: dataId} as data) =>
         if (dataId == txId) {
           Some({
             ...data,
             date: Some(Js.Date.fromFloat(unixTime *. 1000.)),
             status: Confirmed,
           });
         } else {
           None;
         }
       );
  let newUnconf =
    unconfirmedTxs |. List.keep(({txId: dataId}) => dataId != txId);
  {
    ...state,
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
    {
      ...state,
      unconfirmedTxs: [
        {
          txId,
          txType: Payout,
          status: Unconfirmed,
          amount:
            PayoutTransaction.summary(state.network, payoutTx).spentWithFees,
          date: None,
          detailsLink: Venture(state.ventureId, Payout(processId)),
        },
        ...state.unconfirmedTxs,
      ],
    };
  | TransactionConfirmed(event) => state |> mapConfirmation(event)
  | _ => state
  };
