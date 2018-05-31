open Belt;

open PrimitiveTypes;

open Event;

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

type t = {
  network: Network.t,
  localUser: userId,
  payouts: ProcessCollector.collection(data),
  txIdToProcessIdMap: Map.String.t(processId),
  txDates: Map.String.t(Js.Date.t),
};

let make = localUser => {
  network: Regtest,
  localUser,
  payouts: ProcessCollector.make(),
  txIdToProcessIdMap: Map.String.empty,
  txDates: Map.String.empty,
};

let getPayout = (processId, {payouts}) => payouts |. Map.get(processId);

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
  | _ => state
  };
