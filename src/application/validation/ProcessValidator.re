open Belt;

open PrimitiveTypes;

open Event;

type status =
  | InProgress
  | Accepted
  | Denied;

type approvalProcess = {
  status,
  supporterIds: UserId.set,
  rejectorIds: UserId.set,
  eligibleWhenProposing: UserId.set,
  policy: Policy.t,
};

type t = {
  processes: ProcessId.map(approvalProcess),
  currentPartners: UserId.set,
  exists: processId => bool,
  completed: processId => bool,
  isEligible: (processId, userId) => bool,
  didVote: (processId, userId) => bool,
  policyFulfilled: processId => bool,
  canPolicyBeFulfilled: processId => bool,
};

let make = () => {
  processes: ProcessId.makeMap(),
  currentPartners: UserId.emptySet,
  exists: (_) => false,
  completed: (_) => false,
  isEligible: (_, _) => false,
  didVote: (_, _) => false,
  policyFulfilled: (_) => false,
  canPolicyBeFulfilled: (_) => false,
};

let addProposal =
    (
      {processId, policy, supporterId, eligibleWhenProposing}:
        EventTypes.proposal('a),
      map,
    ) =>
  map
  |. Map.set(
       processId,
       {
         status: InProgress,
         supporterIds: [|supporterId|] |> Set.mergeMany(UserId.emptySet),
         rejectorIds: UserId.emptySet,
         eligibleWhenProposing,
         policy,
       },
     );

let addEndorsement = ({processId, supporterId}: EventTypes.endorsement, map) =>
  map
  |. Map.update(
       processId,
       Utils.mapOption(({supporterIds} as process) =>
         {...process, supporterIds: supporterIds |. Set.add(supporterId)}
       ),
     );

let addRejection = ({processId, rejectorId}: EventTypes.rejection, map) =>
  map
  |. Map.update(
       processId,
       Utils.mapOption(({rejectorIds} as process) =>
         {...process, rejectorIds: rejectorIds |. Set.add(rejectorId)}
       ),
     );

let addAcceptance = ({processId}: EventTypes.acceptance('a), map) =>
  map
  |. Map.update(
       processId,
       Utils.mapOption(process => {...process, status: Accepted}),
     );

let addDenial = ({processId}: EventTypes.denial, map) =>
  map
  |. Map.update(
       processId,
       Utils.mapOption(process => {...process, status: Denied}),
     );

let update = (event, {currentPartners, processes} as state) => {
  let {currentPartners, processes} =
    switch (event) {
    | PartnerProposed(proposal) => {
        ...state,
        processes: processes |> addProposal(proposal),
      }
    | PartnerRemovalProposed(proposal) => {
        ...state,
        processes: processes |> addProposal(proposal),
      }
    | AccountCreationProposed(proposal) => {
        ...state,
        processes: processes |> addProposal(proposal),
      }
    | CustodianProposed(proposal) => {
        ...state,
        processes: processes |> addProposal(proposal),
      }
    | CustodianRemovalProposed(proposal) => {
        ...state,
        processes: processes |> addProposal(proposal),
      }
    | PayoutProposed(proposal) => {
        ...state,
        processes: processes |> addProposal(proposal),
      }
    | PartnerEndorsed(endorsement) => {
        ...state,
        processes: processes |> addEndorsement(endorsement),
      }
    | PartnerRemovalEndorsed(endorsement) => {
        ...state,
        processes: processes |> addEndorsement(endorsement),
      }
    | AccountCreationEndorsed(endorsement) => {
        ...state,
        processes: processes |> addEndorsement(endorsement),
      }
    | CustodianEndorsed(endorsement) => {
        ...state,
        processes: processes |> addEndorsement(endorsement),
      }
    | CustodianRemovalEndorsed(endorsement) => {
        ...state,
        processes: processes |> addEndorsement(endorsement),
      }
    | PayoutEndorsed(endorsement) => {
        ...state,
        processes: processes |> addEndorsement(endorsement),
      }
    | PartnerRejected(rejection) => {
        ...state,
        processes: processes |> addRejection(rejection),
      }
    | PartnerRemovalRejected(rejection) => {
        ...state,
        processes: processes |> addRejection(rejection),
      }
    | AccountCreationRejected(rejection) => {
        ...state,
        processes: processes |> addRejection(rejection),
      }
    | CustodianRejected(rejection) => {
        ...state,
        processes: processes |> addRejection(rejection),
      }
    | CustodianRemovalRejected(rejection) => {
        ...state,
        processes: processes |> addRejection(rejection),
      }
    | PayoutRejected(rejection) => {
        ...state,
        processes: processes |> addRejection(rejection),
      }
    | PartnerAccepted({data: {id}} as acceptance) => {
        ...state,
        processes: processes |> addAcceptance(acceptance),
        currentPartners: currentPartners |. Set.add(id),
      }
    | PartnerRemovalAccepted({data: {id}} as acceptance) => {
        ...state,
        processes: processes |> addAcceptance(acceptance),
        currentPartners: currentPartners |. Set.remove(id),
      }
    | AccountCreationAccepted(acceptance) => {
        ...state,
        processes: processes |> addAcceptance(acceptance),
      }
    | CustodianAccepted(acceptance) => {
        ...state,
        processes: processes |> addAcceptance(acceptance),
      }
    | CustodianRemovalAccepted(acceptance) => {
        ...state,
        processes: processes |> addAcceptance(acceptance),
      }
    | PayoutAccepted(acceptance) => {
        ...state,
        processes: processes |> addAcceptance(acceptance),
      }
    | PartnerDenied(denial) => {
        ...state,
        processes: processes |> addDenial(denial),
      }
    | PartnerRemovalDenied(denial) => {
        ...state,
        processes: processes |> addDenial(denial),
      }
    | CustodianDenied(denial) => {
        ...state,
        processes: processes |> addDenial(denial),
      }
    | CustodianRemovalDenied(denial) => {
        ...state,
        processes: processes |> addDenial(denial),
      }
    | PayoutDenied(denial) => {
        ...state,
        processes: processes |> addDenial(denial),
      }
    | VentureCreated(_)
    | PayoutSigned(_)
    | PayoutBroadcast(_)
    | PayoutBroadcastDuplicate(_)
    | PayoutBroadcastFailed(_)
    | CustodianKeyChainUpdated(_)
    | AccountKeyChainIdentified(_)
    | AccountKeyChainActivated(_)
    | IncomeAddressExposed(_)
    | IncomeDetected(_)
    | TransactionConfirmed(_) => state
    };
  {
    processes,
    currentPartners,
    exists: Map.has(processes),
    completed: processId =>
      switch ((processes |. Map.getExn(processId)).status) {
      | Accepted => true
      | _ => false
      },
    isEligible: (processId, partnerId) =>
      (processes |. Map.getExn(processId)).eligibleWhenProposing
      |. Set.has(partnerId),
    didVote: (processId, partnerId) => {
      let {supporterIds, rejectorIds} = processes |. Map.getExn(processId);
      supporterIds |. Set.has(partnerId) || rejectorIds |. Set.has(partnerId);
    },
    policyFulfilled: processId => {
      let {policy, eligibleWhenProposing, supporterIds} =
        processes |. Map.getExn(processId);
      policy
      |> Policy.fulfilled(
           ~eligible=Set.intersect(currentPartners, eligibleWhenProposing),
           ~endorsed=supporterIds,
         );
    },
    canPolicyBeFulfilled: processId => {
      let {policy, eligibleWhenProposing, rejectorIds} =
        processes |. Map.getExn(processId);
      policy
      |> Policy.canBeFulfilled(
           ~eligible=Set.intersect(currentPartners, eligibleWhenProposing),
           ~rejected=rejectorIds,
         );
    },
  };
};
