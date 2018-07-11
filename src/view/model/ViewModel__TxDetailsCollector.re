open Belt;

open PrimitiveTypes;

open Event;

let getExplorerLink = (network, txId) =>
  switch (network) {
  | Network.Mainnet => "https://www.blockchain.com/en/btc/tx/" ++ txId
  | _ => "https://testnet.blockchain.info/tx/" ++ txId
  };

type payoutStatus =
  | PendingApproval
  | Accepted
  | Denied
  | Aborted
  | Unconfirmed
  | Confirmed
  | Failed(string);

type data = {
  payoutStatus,
  summary: PayoutTransaction.summary,
  txId: option(string),
  date: option(Js.Date.t),
};

type payoutProcess = ProcessCollector.process(data);

type incomeStatus =
  | Unconfirmed
  | Confirmed;

type income = {
  status: incomeStatus,
  explorerLink: string,
  date: option(Js.Date.t),
  txId: string,
  amount: BTC.t,
  addresses: Set.String.t,
};

type t = {
  network: Network.t,
  localUser: userId,
  payouts: ProcessCollector.collection(data),
  txIdToProcessIdMap: Map.String.t(processId),
  txDates: Map.String.t(Js.Date.t),
  income: Map.String.t(income),
};

let make = localUser => {
  network: Regtest,
  localUser,
  payouts: ProcessCollector.make(),
  txIdToProcessIdMap: Map.String.empty,
  txDates: Map.String.empty,
  income: Map.String.empty,
};

let getPayout = (processId, {payouts}) => payouts |. Map.get(processId);
let getIncome = (txId, {income}) => income |. Map.String.get(txId);

let getDateAndStatus = (txId, {txDates}) =>
  switch (txDates |. Map.String.get(txId)) {
  | Some(date) => (Some(date), Confirmed)
  | _ => (None, Unconfirmed)
  };

let getProcessIdForTx = (txId, {txIdToProcessIdMap}) =>
  txIdToProcessIdMap |. Map.String.getExn(txId);
let payoutsPendingApproval = ({payouts}) =>
  payouts
  |. Map.valuesToArray
  |> List.fromArray
  |. List.keepU((. payout: payoutProcess) =>
       switch (payout.status) {
       | PendingApproval => true
       | _ => false
       }
     );

let apply = (event, state) =>
  switch (event) {
  | VentureCreated({network}) => {...state, network}
  | PayoutProposed(proposal) => {
      ...state,
      payouts:
        state.payouts
        |> ProcessCollector.addProposal(state.localUser, proposal, data =>
             {
               txId: None,
               date: None,
               payoutStatus: PendingApproval,
               summary:
                 data.payoutTx |> PayoutTransaction.summary(state.network),
             }
           ),
    }
  | PayoutRejected(rejection) => {
      ...state,
      payouts:
        state.payouts
        |> ProcessCollector.addRejection(state.localUser, rejection),
    }
  | PayoutEndorsed(endorsement) => {
      ...state,
      payouts:
        state.payouts
        |> ProcessCollector.addEndorsement(state.localUser, endorsement),
    }
  | PayoutAccepted({processId} as accepted) => {
      ...state,
      payouts:
        state.payouts
        |> ProcessCollector.addAcceptance(accepted)
        |> ProcessCollector.updateData(processId, data =>
             {...data, payoutStatus: Accepted}
           ),
    }
  | PayoutDenied({processId} as denial) => {
      ...state,
      payouts:
        state.payouts
        |> ProcessCollector.addDenial(denial)
        |> ProcessCollector.updateData(processId, data =>
             {...data, payoutStatus: Denied}
           ),
    }
  | PayoutAborted({processId} as abort) => {
      ...state,
      payouts:
        state.payouts
        |> ProcessCollector.addAbort(abort)
        |> ProcessCollector.updateData(processId, data =>
             {...data, payoutStatus: Aborted}
           ),
    }
  | PayoutBroadcast({processId, txId}) =>
    let txDate = state.txDates |. Map.String.get(txId);
    {
      ...state,
      txIdToProcessIdMap:
        state.txIdToProcessIdMap |. Map.String.set(txId, processId),
      payouts:
        state.payouts
        |> ProcessCollector.updateData(processId, data =>
             {
               ...data,
               txId: Some(txId),
               payoutStatus:
                 txDate |> Js.Option.isSome ? Confirmed : Unconfirmed,
               date: txDate,
             }
           ),
    };
  | TransactionConfirmed({txId, unixTime}) =>
    let processId = state.txIdToProcessIdMap |. Map.String.get(txId);
    let txDate = Js.Date.fromFloat(unixTime *. 1000.);
    {
      ...state,
      txDates: state.txDates |. Map.String.set(txId, txDate),
      income:
        state.income
        |. Map.String.update(
             txId,
             Utils.mapOption(income =>
               {...income, date: Some(txDate), status: Confirmed}
             ),
           ),
      payouts:
        switch (processId) {
        | None => state.payouts
        | Some(processId) =>
          state.payouts
          |> ProcessCollector.updateData(processId, data =>
               {...data, date: Some(txDate), payoutStatus: Confirmed}
             )
        },
    };
  | PayoutBroadcastFailed({processId, errorMessage}) => {
      ...state,
      payouts:
        state.payouts
        |> ProcessCollector.updateData(processId, data =>
             {
               ...data,
               payoutStatus:
                 data.payoutStatus != Unconfirmed
                 && data.payoutStatus != Confirmed ?
                   Failed(errorMessage) : data.payoutStatus,
             }
           ),
    }
  | IncomeDetected({address, txId, amount}) =>
    let txDate = state.txDates |. Map.String.get(txId);
    {
      ...state,
      income:
        state.income
        |. Map.String.update(
             txId,
             fun
             | Some(income) =>
               Some({
                 ...income,
                 amount: income.amount |> BTC.plus(amount),
                 addresses: income.addresses |. Set.String.add(address),
               })
             | None =>
               Some({
                 explorerLink: getExplorerLink(state.network, txId),
                 date: txDate,
                 status: txDate |> Js.Option.isSome ? Confirmed : Unconfirmed,
                 txId,
                 amount,
                 addresses: Set.String.empty |. Set.String.add(address),
               }),
           ),
    };
  | _ => state
  };
