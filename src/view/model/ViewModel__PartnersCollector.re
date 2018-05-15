open PrimitiveTypes;

type partner = {
  userId,
  name: option(string),
};

type prospect = {
  processId,
  userId,
  endorsedBy: list(userId),
};

type t = {
  partners: list(partner),
  prospects: list(prospect),
  removalProspects: list(prospect),
  partnerPolicy: Policy.t,
};

let make = () => {
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
      partners: [{userId: data.id, name: None}, ...state.partners],
      prospects:
        state.prospects |> List.filter(p => UserId.neq(p.userId, data.id)),
    }
  | PartnerRemovalProposed({processId, supporterId, data}) => {
      ...state,
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
