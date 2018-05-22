open Belt;

open PrimitiveTypes;

open Event;

type payoutStatus =
  | PendingApproval
  | Accepted
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
  txIds: Set.String.t,
};

let make = localUser => {
  network: Regtest,
  localUser,
  payouts: ProcessCollector.make(),
  txIdToProcessIdMap: Map.String.empty,
  txIds: Set.String.empty,
};

let getPayout = (processId, {payouts}) => payouts |. Map.getExn(processId);

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
  | PayoutBroadcast({processId, txId}) => {
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
                 state.txIds |. Set.String.has(txId) ?
                   Confirmed : Unconfirmed,
             }
           ),
    }
  | TransactionConfirmed({txId, unixTime}) =>
    let processId = state.txIdToProcessIdMap |. Map.String.get(txId);
    {
      ...state,
      txIds: state.txIds |. Set.String.add(txId),
      payouts:
        switch (processId) {
        | None => state.payouts
        | Some(processId) =>
          state.payouts
          |> ProcessCollector.updateData(processId, data =>
               {
                 ...data,
                 date: Some(Js.Date.fromFloat(unixTime *. 1000.)),
                 payoutStatus: Confirmed,
               }
             )
        },
    };
  | PayoutBroadcastFailed({processId, errorMessage}) => {
      ...state,
      payouts:
        state.payouts
        |> ProcessCollector.updateData(processId, data =>
             {...data, payoutStatus: Failed(errorMessage)}
           ),
    }
  | _ => state
  };
