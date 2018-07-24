open Belt;

open PrimitiveTypes;

type partner = {
  userId,
  processId,
  name: option(string),
  canProposeRemoval: bool,
  hasLoggedIn: Js.Promise.t(bool),
  joinedWallet: bool,
};

type processType =
  | Removal
  | Addition;

type data = {
  userId,
  processType,
  hasLoggedIn: Js.Promise.t(bool),
};

type partnerProcess = ProcessCollector.process(data);

type t = {
  localUser: userId,
  partners: list(partner),
  prospects: ProcessCollector.collection(data),
  partnerPolicy: Policy.t,
};

let getProspect = (processId, {prospects}) =>
  prospects |. Map.get(processId);

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

let hasUserLoggedIn = (pubKey, userId) =>
  Js.Promise.(
    switch (pubKey) {
    | Some(_) => true |> resolve
    | None =>
      UserInfo.Public.(
        read(~blockstackId=userId)
        |> then_(
             fun
             | NotFound => false |> resolve
             | _ => true |> resolve,
           )
      )
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
  | CustodianKeyChainUpdated({custodianId}) => {
      ...state,
      partners:
        state.partners
        |. List.mapU((. partner: partner) =>
             {
               ...partner,
               joinedWallet:
                 partner.joinedWallet
                 || UserId.eq(partner.userId, custodianId),
             }
           ),
    }
  | PartnerProposed(proposal) => {
      ...state,
      prospects:
        state.prospects
        |> ProcessCollector.addProposal(state.localUser, proposal, data =>
             {
               userId: data.id,
               processType: Addition,
               hasLoggedIn:
                 hasUserLoggedIn(proposal.data.pubKey, proposal.data.id),
             }
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
  | PartnerAccepted({processId, data} as acceptance) => {
      ...state,
      partners: [
        {
          userId: data.id,
          processId,
          name: None,
          canProposeRemoval: UserId.neq(data.id, state.localUser),
          hasLoggedIn: hasUserLoggedIn(data.pubKey, data.id),
          joinedWallet: false,
        },
        ...state.partners
           |. List.keepU((. {userId}: partner) =>
                UserId.neq(userId, data.id)
              ),
      ],
      prospects:
        state.prospects |> ProcessCollector.addAcceptance(acceptance),
    }
  | PartnerPubKeyAdded({partnerId}) => {
      ...state,
      partners:
        state.partners
        |. List.mapU((. partner: partner) =>
             {
               ...partner,
               hasLoggedIn:
                 Js.Promise.(
                   partner.hasLoggedIn
                   |> then_(known =>
                        resolve(
                          known || UserId.eq(partner.userId, partnerId),
                        )
                      )
                 ),
             }
           ),
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
             {
               userId: data.id,
               processType: Removal,
               hasLoggedIn:
                 (
                   state.partners
                   |. List.getByU((. p: partner) =>
                        UserId.eq(p.userId, data.id)
                      )
                   |> Js.Option.getExn
                 ).
                   hasLoggedIn,
             }
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
