open PrimitiveTypes;

type partner = {
  userId,
  name: option(string),
  canProposeRemoval: bool,
};

type prospect = {
  processId,
  userId,
  endorsedBy: list(userId),
};

type t = {
  localUser: userId,
  partners: list(partner),
  prospects: list(prospect),
  removalProspects: list(prospect),
  partnerPolicy: Policy.t,
};

let make = localUser => {
  localUser,
  partners: [],
  prospects: [],
  removalProspects: [],
  partnerPolicy: Policy.Unanimous,
};

let apply = (event: Event.t, state) =>
  switch (event) {
  | VentureCreated({metaPolicy}) => {...state, partnerPolicy: metaPolicy}
  | PartnerProposed({processId, supporterId, data}) => {
      ...state,
      prospects: [
        {processId, userId: data.id, endorsedBy: [supporterId]},
        ...state.prospects,
      ],
    }
  | PartnerEndorsed({processId, supporterId}) => {
      ...state,
      prospects:
        state.prospects
        |> List.map((p: prospect) =>
             ProcessId.eq(p.processId, processId) ?
               {...p, endorsedBy: [supporterId, ...p.endorsedBy]} : p
           ),
    }
  | PartnerAccepted({data}) => {
      ...state,
      partners: [
        {
          userId: data.id,
          name: None,
          canProposeRemoval: UserId.neq(data.id, state.localUser),
        },
        ...state.partners,
      ],
      prospects:
        state.prospects |> List.filter(p => UserId.neq(p.userId, data.id)),
    }
  | PartnerRemovalProposed({processId, supporterId, data}) => {
      ...state,
      partners:
        state.partners
        |> List.map((p: partner) =>
             UserId.eq(p.userId, data.id) ?
               {...p, canProposeRemoval: false} : p
           ),
      removalProspects: [
        {processId, userId: data.id, endorsedBy: [supporterId]},
        ...state.removalProspects,
      ],
    }
  | PartnerRemovalEndorsed({processId, supporterId}) => {
      ...state,
      removalProspects:
        state.removalProspects
        |> List.map((p: prospect) =>
             ProcessId.eq(p.processId, processId) ?
               {...p, endorsedBy: [supporterId, ...p.endorsedBy]} : p
           ),
    }
  | PartnerRemovalAccepted({processId, data: {id}}) => {
      ...state,
      partners:
        state.partners
        |> List.filter((p: partner) => UserId.neq(p.userId, id)),
      removalProspects:
        state.removalProspects
        |> List.filter((p: prospect) =>
             ProcessId.neq(p.processId, processId)
           ),
    }
  | _ => state
  };

let partners = state => state.partners;

let prospects = state => state.prospects;

let removalProspects = state => state.removalProspects;

let isPartner = (id, {partners}) =>
  partners |> List.exists(({userId}: partner) => UserId.eq(userId, id));
