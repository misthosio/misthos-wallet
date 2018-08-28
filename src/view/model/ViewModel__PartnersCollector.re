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
  joinedWallet: bool,
};

type partnerProcess = ProcessCollector.process(data);

type t = {
  localUser: userId,
  partners: list(partner),
  partnerProcesses: ProcessCollector.collection(data),
  partnerPolicy: Policy.t,
  everJoinedWallet: UserId.set,
};

let currentPartners = ({partners}) =>
  partners
  |. List.reduceU(UserId.emptySet, (. set, {userId}: partner) =>
       set |. Set.add(userId)
     );

let getPartnerProcess = (processId, {partnerProcesses}) =>
  partnerProcesses |. Map.get(processId);

let processesPendingApproval = ({partnerProcesses}) =>
  partnerProcesses
  |. Map.valuesToArray
  |> List.fromArray
  |. List.keepU((. prospect: partnerProcess) =>
       switch (prospect.status) {
       | PendingApproval => true
       | _ => false
       }
     )
  |. List.reduceReverseU(
       ([], []), (. (additions, removals), process: partnerProcess) =>
       process.data.processType == Addition ?
         ([process, ...additions], removals) :
         (additions, [process, ...removals])
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
  partnerProcesses: ProcessId.makeMap(),
  partnerPolicy: Policy.Unanimous,
  everJoinedWallet: UserId.emptySet,
};

let apply = (event: Event.t, state) =>
  switch (event) {
  | VentureCreated({initialPolicies}) => {
      ...state,
      partnerPolicy:
        initialPolicies
        |> Utils.mapOption((p: Policy.initialPolicies) => p.addPartner)
        |> Js.Option.getWithDefault(Policy.defaultAddPartner),
    }
  | CustodianKeyChainUpdated({custodianId}) =>
    let partner =
      state.partners
      |. List.getByU((. partner: partner) =>
           UserId.eq(partner.userId, custodianId)
         )
      |> Js.Option.getExn;
    {
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
      partnerProcesses:
        state.partnerProcesses
        |> ProcessCollector.updateData(partner.processId, data =>
             {...data, joinedWallet: true}
           ),
      everJoinedWallet: state.everJoinedWallet |. Set.add(custodianId),
    };
  | PartnerProposed(proposal) => {
      ...state,
      partnerProcesses:
        state.partnerProcesses
        |> ProcessCollector.addProposal(state.localUser, proposal, data =>
             {
               userId: data.id,
               joinedWallet: state.everJoinedWallet |. Set.has(data.id),
               processType: Addition,
               hasLoggedIn:
                 hasUserLoggedIn(proposal.data.pubKey, proposal.data.id),
             }
           ),
    }
  | PartnerRejected(rejection) => {
      ...state,
      partnerProcesses:
        state.partnerProcesses
        |> ProcessCollector.addRejection(state.localUser, rejection),
    }
  | PartnerEndorsed(endorsement) => {
      ...state,
      partnerProcesses:
        state.partnerProcesses
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
          joinedWallet: state.everJoinedWallet |. Set.has(data.id),
        },
        ...state.partners
           |. List.keepU((. {userId}: partner) =>
                UserId.neq(userId, data.id)
              ),
      ],
      partnerProcesses:
        state.partnerProcesses |> ProcessCollector.addAcceptance(acceptance),
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
      partnerProcesses:
        state.partnerProcesses |> ProcessCollector.addDenial(denial),
    }
  | PartnerRemovalProposed(proposal) =>
    let partner =
      state.partners
      |. List.getByU((. p: partner) =>
           UserId.eq(p.userId, proposal.data.id)
         )
      |> Js.Option.getExn;
    {
      ...state,
      partners:
        state.partners
        |. List.map((p: partner) =>
             UserId.eq(p.userId, proposal.data.id) ?
               {...p, canProposeRemoval: false} : p
           ),
      partnerProcesses:
        state.partnerProcesses
        |> ProcessCollector.addProposal(state.localUser, proposal, data =>
             {
               userId: data.id,
               processType: Removal,
               joinedWallet: partner.joinedWallet,
               hasLoggedIn: partner.hasLoggedIn,
             }
           ),
    };
  | PartnerRemovalRejected(rejection) => {
      ...state,
      partnerProcesses:
        state.partnerProcesses
        |> ProcessCollector.addRejection(state.localUser, rejection),
    }
  | PartnerRemovalEndorsed(endorsement) => {
      ...state,
      partnerProcesses:
        state.partnerProcesses
        |> ProcessCollector.addEndorsement(state.localUser, endorsement),
    }
  | PartnerRemovalAccepted({data: {id}} as acceptance) => {
      ...state,
      partners:
        state.partners |. List.keep((p: partner) => UserId.neq(p.userId, id)),
      partnerProcesses:
        state.partnerProcesses |> ProcessCollector.addAcceptance(acceptance),
    }
  | PartnerRemovalDenied(denial) => {
      ...state,
      partnerProcesses:
        state.partnerProcesses |> ProcessCollector.addDenial(denial),
    }
  | _ => state
  };

let isPartner = (id, {partners}) =>
  partners |. List.some(({userId}: partner) => UserId.eq(userId, id));
