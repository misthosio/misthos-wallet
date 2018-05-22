open Belt;

open PrimitiveTypes;

open Event;

type payoutStatus =
  | PendingApproval
  | Accepted
  | Unconfirmed
  | Confirmed
  | Failed(string);

type voteStatus =
  | Pending
  | Endorsed
  | Rejected;

type voter = {
  userId,
  voteStatus,
};

type payout = {
  processId,
  status: payoutStatus,
  canEndorse: bool,
  canReject: bool,
  summary: PayoutTransaction.summary,
  voters: list(voter),
  txId: option(string),
  date: option(Js.Date.t),
};

type t = {
  network: Network.t,
  localUser: userId,
  payouts: ProcessId.map(payout),
  txIdToProcessIdMap: Map.String.t(processId),
  txIds: Set.String.t,
};

let make = localUser => {
  network: Regtest,
  localUser,
  payouts: ProcessId.makeMap(),
  txIdToProcessIdMap: Map.String.empty,
  txIds: Set.String.empty,
};

let getPayout = (processId, {payouts}) => payouts |. Map.getExn(processId);

let payoutsPendingApproval = ({payouts}) =>
  payouts
  |. Map.valuesToArray
  |> List.fromArray
  |. List.keepU((. payout) =>
       switch (payout.status) {
       | PendingApproval => true
       | _ => false
       }
     );

let apply = (event, state) =>
  switch (event) {
  | VentureCreated({network}) => {...state, network}
  | PayoutProposed({processId, eligibleWhenProposing, supporterId, data}) => {
      ...state,
      payouts:
        state.payouts
        |. Map.set(
             processId,
             {
               processId,
               txId: None,
               date: None,
               canEndorse:
                 UserId.neq(supporterId, state.localUser)
                 && eligibleWhenProposing
                 |. Set.has(state.localUser),
               canReject:
                 UserId.neq(supporterId, state.localUser)
                 && eligibleWhenProposing
                 |. Set.has(state.localUser),
               status: PendingApproval,
               summary:
                 data.payoutTx |> PayoutTransaction.summary(state.network),
               voters:
                 eligibleWhenProposing
                 |> Set.toList
                 |. List.mapU((. userId) =>
                      {
                        userId,
                        voteStatus:
                          UserId.eq(supporterId, userId) ? Endorsed : Pending,
                      }
                    ),
             },
           ),
    }
  | PayoutRejected({processId, rejectorId}) => {
      ...state,
      payouts:
        state.payouts
        |. Map.update(
             processId,
             Utils.mapOption(payout =>
               {
                 ...payout,
                 canEndorse:
                   payout.canEndorse
                   && UserId.neq(rejectorId, state.localUser),
                 canReject:
                   payout.canReject && UserId.neq(rejectorId, state.localUser),
                 voters:
                   payout.voters
                   |. List.mapU((. {userId, voteStatus}) =>
                        UserId.eq(userId, rejectorId) ?
                          {userId, voteStatus: Rejected} :
                          {userId, voteStatus}
                      ),
               }
             ),
           ),
    }
  | PayoutEndorsed({processId, supporterId}) => {
      ...state,
      payouts:
        state.payouts
        |. Map.update(
             processId,
             Utils.mapOption(payout =>
               {
                 ...payout,
                 canEndorse:
                   payout.canEndorse
                   && UserId.neq(supporterId, state.localUser),
                 canReject:
                   payout.canReject
                   && UserId.neq(supporterId, state.localUser),
                 voters:
                   payout.voters
                   |. List.mapU((. {userId, voteStatus}) =>
                        UserId.eq(userId, supporterId) ?
                          {userId, voteStatus: Endorsed} :
                          {userId, voteStatus}
                      ),
               }
             ),
           ),
    }
  | PayoutAccepted({processId}) => {
      ...state,
      payouts:
        state.payouts
        |. Map.update(
             processId,
             Utils.mapOption(payout =>
               {
                 ...payout,
                 canEndorse: false,
                 canReject: false,
                 status: Accepted,
               }
             ),
           ),
    }
  | PayoutBroadcast({processId, txId}) => {
      ...state,
      txIdToProcessIdMap:
        state.txIdToProcessIdMap |. Map.String.set(txId, processId),
      payouts:
        state.payouts
        |. Map.update(
             processId,
             Utils.mapOption(payout =>
               {
                 ...payout,
                 txId: Some(txId),
                 status:
                   state.txIds |. Set.String.has(txId) ?
                     Confirmed : Unconfirmed,
               }
             ),
           ),
    }
  | TransactionConfirmed({txId, unixTime}) =>
    let processId = state.txIdToProcessIdMap |. Map.String.get(txId);
    switch (processId) {
    | None => {...state, txIds: state.txIds |. Set.String.add(txId)}
    | Some(processId) => {
        ...state,
        txIds: state.txIds |. Set.String.add(txId),
        payouts:
          state.payouts
          |. Map.update(
               processId,
               Utils.mapOption(payout =>
                 {
                   ...payout,
                   date: Some(Js.Date.fromFloat(unixTime *. 1000.)),
                   status: Confirmed,
                 }
               ),
             ),
      }
    };
  | PayoutBroadcastFailed({processId, errorMessage}) => {
      ...state,
      payouts:
        state.payouts
        |. Map.update(
             processId,
             Utils.mapOption(payout =>
               {...payout, status: Failed(errorMessage)}
             ),
           ),
    }
  | _ => state
  };
