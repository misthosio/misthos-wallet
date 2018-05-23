open Belt;

open PrimitiveTypes;

open EventTypes;

type voteStatus =
  | Pending
  | Endorsed
  | Rejected;

type voter = {
  userId,
  voteStatus,
};

type status =
  | PendingApproval
  | Accepted
  | Denied;

type process('data) = {
  processId,
  status,
  proposedBy: userId,
  canVote: bool,
  voters: list(voter),
  data: 'data,
};

type collection('data) = ProcessId.map(process('data));

let make = ProcessId.makeMap;

let addProposal =
    (
      localUser,
      {eligibleWhenProposing, processId, supporterId, data}: proposal('a),
      makeData: 'a => 'data,
      map,
    ) =>
  map
  |. Map.set(
       processId,
       {
         processId,
         status: PendingApproval,
         proposedBy: supporterId,
         canVote:
           UserId.neq(supporterId, localUser)
           && eligibleWhenProposing
           |. Set.has(localUser),
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
         data: makeData(data),
       },
     );

let addRejection = (localUser, {processId, rejectorId}: rejection, map) =>
  map
  |. Map.update(
       processId,
       Utils.mapOption(process =>
         {
           ...process,
           canVote: process.canVote && UserId.neq(rejectorId, localUser),
           voters:
             process.voters
             |. List.mapU((. {userId, voteStatus}) =>
                  UserId.eq(userId, rejectorId) ?
                    {userId, voteStatus: Rejected} : {userId, voteStatus}
                ),
         }
       ),
     );

let addEndorsement = (localUser, {processId, supporterId}: endorsement, map) =>
  map
  |. Map.update(
       processId,
       Utils.mapOption(process =>
         {
           ...process,
           canVote: process.canVote && UserId.neq(supporterId, localUser),
           voters:
             process.voters
             |. List.mapU((. {userId, voteStatus}) =>
                  UserId.eq(userId, supporterId) ?
                    {userId, voteStatus: Endorsed} : {userId, voteStatus}
                ),
         }
       ),
     );

let addAcceptance = ({processId}: acceptance('a), map) =>
  map
  |. Map.update(
       processId,
       Utils.mapOption(payout =>
         {...payout, canVote: false, status: Accepted}
       ),
     );

let addDenial = ({processId}: denial, map) =>
  map
  |. Map.update(
       processId,
       Utils.mapOption(payout => {...payout, canVote: false, status: Denied}),
     );

let updateData = (processId, fn, map) =>
  map
  |. Map.update(
       processId,
       Utils.mapOption(process => {...process, data: fn(process.data)}),
     );
