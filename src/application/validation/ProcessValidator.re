open Belt;

open PrimitiveTypes;

open Event;

type status =
  | InProgress
  | Accepted;

type approvalProcess = {
  status,
  supporterIds: UserId.set,
  rejectorIds: UserId.set,
  policy: Policy.t,
};

type t = {processes: ProcessId.map(approvalProcess)};

let make = () => {processes: ProcessId.makeMap()};

let addProposal =
    ({processId, policy, supporterId}: EventTypes.proposal('a), map) =>
  map
  |. Map.set(
       processId,
       {
         status: InProgress,
         supporterIds: [|supporterId|] |> Set.mergeMany(UserId.emptySet),
         rejectorIds: UserId.emptySet,
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

let update = (event, {processes}) => {
  let processes =
    switch (event) {
    | PartnerProposed(proposal) => processes |> addProposal(proposal)
    | PartnerRemovalProposed(proposal) => processes |> addProposal(proposal)
    | AccountCreationProposed(proposal) => processes |> addProposal(proposal)
    | CustodianProposed(proposal) => processes |> addProposal(proposal)
    | CustodianRemovalProposed(proposal) =>
      processes |> addProposal(proposal)
    | PayoutProposed(proposal) => processes |> addProposal(proposal)
    | PartnerEndorsed(endorsement) =>
      processes |> addEndorsement(endorsement)
    | PartnerRemovalEndorsed(endorsement) =>
      processes |> addEndorsement(endorsement)
    | AccountCreationEndorsed(endorsement) =>
      processes |> addEndorsement(endorsement)
    | CustodianEndorsed(endorsement) =>
      processes |> addEndorsement(endorsement)
    | CustodianRemovalEndorsed(endorsement) =>
      processes |> addEndorsement(endorsement)
    | PayoutEndorsed(endorsement) => processes |> addEndorsement(endorsement)
    | PartnerRejected(rejection) => processes |> addRejection(rejection)
    | PartnerRemovalRejected(rejection) =>
      processes |> addRejection(rejection)
    | AccountCreationRejected(rejection) =>
      processes |> addRejection(rejection)
    | CustodianRejected(rejection) => processes |> addRejection(rejection)
    | CustodianRemovalRejected(rejection) =>
      processes |> addRejection(rejection)
    | PayoutRejected(rejection) => processes |> addRejection(rejection)
    | PartnerAccepted(acceptance) => processes |> addAcceptance(acceptance)
    | PartnerRemovalAccepted(acceptance) =>
      processes |> addAcceptance(acceptance)
    | AccountCreationAccepted(acceptance) =>
      processes |> addAcceptance(acceptance)
    | CustodianAccepted(acceptance) => processes |> addAcceptance(acceptance)
    | CustodianRemovalAccepted(acceptance) =>
      processes |> addAcceptance(acceptance)
    | PayoutAccepted(acceptance) => processes |> addAcceptance(acceptance)
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
    | TransactionConfirmed(_) => processes
    };
  {processes: processes};
};
