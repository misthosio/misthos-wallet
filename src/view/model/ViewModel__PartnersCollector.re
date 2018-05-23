open Belt;

open PrimitiveTypes;

type partner = {
  userId,
  name: option(string),
  canProposeRemoval: bool,
};

type processType =
  | Removal
  | Addition;

type data = {
  userId,
  processType,
};

type partnerProcess = ProcessCollector.process(data);

type t = {
  localUser: userId,
  partners: list(partner),
  prospects: ProcessCollector.collection(data),
  partnerPolicy: Policy.t,
};

let getProspect = (processId, {prospects}) =>
  prospects |. Map.getExn(processId);

let prospectsPendingApproval = ({prospects}) =>
  prospects
  |. Map.valuesToArray
  |> List.fromArray
  |. List.keepU((. prospect: partnerProcess) =>
       switch (prospect.status) {
       | PendingApproval => true
       | _ => false
       }
     );

let make = localUser => {
  localUser,
  partners: [],
  prospects: ProcessId.makeMap(),
  partnerPolicy: Policy.Unanimous,
};

let apply = (event: Event.t, state) =>
  switch (event) {
  | VentureCreated({metaPolicy}) => {...state, partnerPolicy: metaPolicy}
  | PartnerProposed(proposal) => {
      ...state,
      prospects:
        state.prospects
        |> ProcessCollector.addProposal(state.localUser, proposal, data =>
             {userId: data.id, processType: Addition}
           ),
    }
  | PartnerRejected(rejection) => {
      ...state,
      prospects:
        state.prospects
        |> ProcessCollector.addRejection(state.localUser, rejection),
    }
  | PartnerEndorsed(endorsement) => {
      ...state,
      prospects:
        state.prospects
        |> ProcessCollector.addEndorsement(state.localUser, endorsement),
    }
  | PartnerAccepted({data} as acceptance) => {
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
        state.prospects |> ProcessCollector.addAcceptance(acceptance),
    }
  | PartnerDenied(denial) => {
      ...state,
      prospects: state.prospects |> ProcessCollector.addDenial(denial),
    }
  | PartnerRemovalProposed(proposal) => {
      ...state,
      partners:
        state.partners
        |. List.map((p: partner) =>
             UserId.eq(p.userId, proposal.data.id) ?
               {...p, canProposeRemoval: false} : p
           ),
      prospects:
        state.prospects
        |> ProcessCollector.addProposal(state.localUser, proposal, data =>
             {userId: data.id, processType: Removal}
           ),
    }
  | PartnerRemovalRejected(rejection) => {
      ...state,
      prospects:
        state.prospects
        |> ProcessCollector.addRejection(state.localUser, rejection),
    }
  | PartnerRemovalEndorsed(endorsement) => {
      ...state,
      prospects:
        state.prospects
        |> ProcessCollector.addEndorsement(state.localUser, endorsement),
    }
  | PartnerRemovalAccepted({data: {id}} as acceptance) => {
      ...state,
      partners:
        state.partners |. List.keep((p: partner) => UserId.neq(p.userId, id)),
      prospects:
        state.prospects |> ProcessCollector.addAcceptance(acceptance),
    }
  | PartnerRemovalDenied(denial) => {
      ...state,
      prospects: state.prospects |> ProcessCollector.addDenial(denial),
    }
  | _ => state
  };

let isPartner = (id, {partners}) =>
  partners |. List.some(({userId}: partner) => UserId.eq(userId, id));
