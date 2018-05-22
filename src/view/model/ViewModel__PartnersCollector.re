open Belt;

open PrimitiveTypes;

type partner = {
  userId,
  name: option(string),
  canProposeRemoval: bool,
};

type voteStatus =
  | Pending
  | Endorsed
  | Rejected;

type voter = {
  userId,
  voteStatus,
};

type processType =
  | Removal
  | Addition;

type prospect = {
  processId,
  userId,
  processType,
  voters: list(voter),
  canEndorse: bool,
  canReject: bool,
};

type t = {
  localUser: userId,
  partners: list(partner),
  currentProcesses: UserId.map(processId),
  prospects: ProcessId.map(prospect),
  partnerPolicy: Policy.t,
};

let getProspect = (userId, {currentProcesses, prospects}) =>
  currentProcesses |. Map.getExn(userId) |> Map.getExn(prospects);

let prospectsPendingApproval = ({prospects}) =>
  prospects |. Map.valuesToArray |> List.fromArray;

let make = localUser => {
  localUser,
  partners: [],
  currentProcesses: UserId.makeMap(),
  prospects: ProcessId.makeMap(),
  partnerPolicy: Policy.Unanimous,
};

let apply = (event: Event.t, state) =>
  switch (event) {
  | VentureCreated({metaPolicy}) => {...state, partnerPolicy: metaPolicy}
  | PartnerProposed({eligibleWhenProposing, processId, supporterId, data}) => {
      ...state,
      currentProcesses: state.currentProcesses |. Map.set(data.id, processId),
      prospects:
        state.prospects
        |. Map.set(
             processId,
             {
               processType: Addition,
               canEndorse:
                 UserId.neq(supporterId, state.localUser)
                 && eligibleWhenProposing
                 |. Set.has(state.localUser),
               canReject:
                 UserId.neq(supporterId, state.localUser)
                 && eligibleWhenProposing
                 |. Set.has(state.localUser),
               processId,
               userId: data.id,
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
  | PartnerRejected({processId, rejectorId}) => {
      ...state,
      prospects:
        state.prospects
        |. Map.update(
             processId,
             Utils.mapOption(prospect =>
               {
                 ...prospect,
                 canEndorse:
                   prospect.canEndorse
                   && UserId.neq(rejectorId, state.localUser),
                 canReject:
                   prospect.canReject
                   && UserId.neq(rejectorId, state.localUser),
                 voters:
                   prospect.voters
                   |. List.mapU((. {userId, voteStatus}) =>
                        UserId.eq(userId, rejectorId) ?
                          {userId, voteStatus: Rejected} :
                          {userId, voteStatus}
                      ),
               }
             ),
           ),
    }
  | PartnerEndorsed({processId, supporterId}) => {
      ...state,
      prospects:
        state.prospects
        |. Map.update(
             processId,
             Utils.mapOption(prospect =>
               {
                 ...prospect,
                 canEndorse:
                   prospect.canEndorse
                   && UserId.neq(supporterId, state.localUser),
                 canReject:
                   prospect.canReject
                   && UserId.neq(supporterId, state.localUser),
                 voters:
                   prospect.voters
                   |. List.mapU((. {userId, voteStatus}) =>
                        UserId.eq(userId, supporterId) ?
                          {userId, voteStatus: Endorsed} :
                          {userId, voteStatus}
                      ),
               }
             ),
           ),
    }
  | PartnerAccepted({processId, data}) => {
      ...state,
      partners: [
        {
          userId: data.id,
          name: None,
          canProposeRemoval: UserId.neq(data.id, state.localUser),
        },
        ...state.partners,
      ],
      prospects: state.prospects |. Map.remove(processId),
      currentProcesses: state.currentProcesses |. Map.remove(data.id),
    }
  | PartnerRemovalProposed({
      eligibleWhenProposing,
      processId,
      supporterId,
      data,
    }) => {
      ...state,
      partners:
        state.partners
        |. List.map((p: partner) =>
             UserId.eq(p.userId, data.id) ?
               {...p, canProposeRemoval: false} : p
           ),
      currentProcesses: state.currentProcesses |. Map.set(data.id, processId),
      prospects:
        state.prospects
        |. Map.set(
             processId,
             {
               processType: Removal,
               canEndorse:
                 UserId.neq(supporterId, state.localUser)
                 && eligibleWhenProposing
                 |. Set.has(state.localUser),
               canReject:
                 UserId.neq(supporterId, state.localUser)
                 && eligibleWhenProposing
                 |. Set.has(state.localUser),
               processId,
               userId: data.id,
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
  | PartnerRemovalRejected({processId, rejectorId}) => {
      ...state,
      prospects:
        state.prospects
        |. Map.update(
             processId,
             Utils.mapOption(prospect =>
               {
                 ...prospect,
                 canEndorse:
                   prospect.canEndorse
                   && UserId.neq(rejectorId, state.localUser),
                 canReject:
                   prospect.canReject
                   && UserId.neq(rejectorId, state.localUser),
                 voters:
                   prospect.voters
                   |. List.mapU((. {userId, voteStatus}) =>
                        UserId.eq(userId, rejectorId) ?
                          {userId, voteStatus: Rejected} :
                          {userId, voteStatus}
                      ),
               }
             ),
           ),
    }
  | PartnerRemovalEndorsed({processId, supporterId}) => {
      ...state,
      prospects:
        state.prospects
        |. Map.update(
             processId,
             Utils.mapOption(prospect =>
               {
                 ...prospect,
                 canEndorse:
                   prospect.canEndorse
                   && UserId.neq(supporterId, state.localUser),
                 canReject:
                   prospect.canReject
                   && UserId.neq(supporterId, state.localUser),
                 voters:
                   prospect.voters
                   |. List.mapU((. {userId, voteStatus}) =>
                        UserId.eq(userId, supporterId) ?
                          {userId, voteStatus: Endorsed} :
                          {userId, voteStatus}
                      ),
               }
             ),
           ),
    }
  | PartnerRemovalAccepted({processId, data: {id}}) => {
      ...state,
      partners:
        state.partners |. List.keep((p: partner) => UserId.neq(p.userId, id)),
      prospects: state.prospects |. Map.remove(processId),
      currentProcesses: state.currentProcesses |. Map.remove(id),
    }
  | _ => state
  };

let isPartner = (id, {partners}) =>
  partners |. List.some(({userId}: partner) => UserId.eq(userId, id));
