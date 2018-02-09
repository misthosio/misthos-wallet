open PrimitiveTypes;

open Event;

type approvalProcess = {
  supporterIds: list(userId),
  policy: Policy.t
};

type state = {
  ventureName: string,
  systemPubKey: string,
  metaPolicy: Policy.t,
  partnerIds: list(userId),
  partnerAddresses: list(string),
  partnerPubKeys: list((string, userId)),
  partnerData: list((processId, Partner.Data.t)),
  partnerLabelData: list((processId, PartnerLabel.Data.t)),
  contributionData: list((processId, Contribution.Data.t)),
  processes: list((processId, approvalProcess)),
  policies: list((string, Policy.t))
};

let makeState = () => {
  ventureName: "",
  systemPubKey: "",
  partnerIds: [],
  partnerAddresses: [],
  partnerPubKeys: [],
  partnerLabelData: [],
  metaPolicy: Policy.absolute,
  partnerData: [],
  contributionData: [],
  processes: [],
  policies: []
};

let addProcess =
    (
      {processId, policy, supporterId}: EventTypes.proposal('a),
      {processes} as state
    ) => {
  ...state,
  processes: [(processId, {policy, supporterIds: [supporterId]}), ...processes]
};

let endorseProcess =
    ({processId, supporterId}: EventTypes.endorsement, {processes} as state) => {
  ...state,
  processes:
    processes
    |> List.map(((pId, process)) =>
         ProcessId.eq(pId, processId) ?
           (
             pId,
             {...process, supporterIds: [supporterId, ...process.supporterIds]}
           ) :
           (pId, process)
       )
};

let apply = (event: Event.t, state) =>
  switch event {
  | VentureCreated({
      ventureName,
      creatorId,
      creatorPubKey,
      metaPolicy,
      systemIssuer
    }) => {
      ...state,
      ventureName,
      partnerIds: [creatorId, ...state.partnerIds],
      partnerAddresses: [
        Utils.addressFromPublicKey(creatorPubKey),
        ...state.partnerAddresses
      ],
      partnerPubKeys: [(creatorPubKey, creatorId), ...state.partnerPubKeys],
      systemPubKey: systemIssuer |> Utils.publicKeyFromKeyPair,
      metaPolicy,
      policies:
        [
          Partner.processName,
          PartnerLabel.processName,
          Contribution.processName
        ]
        |> List.map(n => (n, metaPolicy))
    }
  | PartnerProposed({processId, data} as proposal) => {
      ...addProcess(proposal, state),
      partnerData: [(processId, data), ...state.partnerData]
    }
  | PartnerLabelProposed({processId, data} as proposal) => {
      ...addProcess(proposal, state),
      partnerLabelData: [(processId, data), ...state.partnerLabelData]
    }
  | ContributionProposed({processId, data} as proposal) => {
      ...addProcess(proposal, state),
      contributionData: [(processId, data), ...state.contributionData]
    }
  | PartnerEndorsed(endorsement) => endorseProcess(endorsement, state)
  | PartnerLabelEndorsed(endorsement) => endorseProcess(endorsement, state)
  | PartnerAccepted({data}) => {
      ...state,
      partnerIds: [data.id, ...state.partnerIds],
      partnerAddresses: [
        Utils.addressFromPublicKey(data.pubKey),
        ...state.partnerAddresses
      ],
      partnerPubKeys: [(data.pubKey, data.id), ...state.partnerPubKeys]
    }
  | PartnerLabelAccepted(_) => state
  | ContributionEndorsed(endorsement) => endorseProcess(endorsement, state)
  | ContributionAccepted(_) => state
  };

type result =
  | Ok
  | InvalidIssuer
  | UnknownProcessId
  | BadData
  | DuplicateEndorsement
  | PolicyMissmatch
  | PolicyNotFulfilled;

let validateProposal =
    (
      {policy, supporterId}: EventTypes.proposal('a),
      processName,
      {policies, partnerPubKeys},
      issuerPubKey
    ) =>
  if (Policy.neq(policy, policies |> List.assoc(processName))) {
    PolicyMissmatch;
  } else if (UserId.neq(
               partnerPubKeys |> List.assoc(issuerPubKey),
               supporterId
             )) {
    InvalidIssuer;
  } else {
    Ok;
  };

let validateEndorsement =
    (
      {processId, supporterId}: EventTypes.endorsement,
      {processes, partnerPubKeys},
      issuerPubKey
    ) =>
  try {
    let {supporterIds} = processes |> List.assoc(processId);
    if (UserId.neq(partnerPubKeys |> List.assoc(issuerPubKey), supporterId)) {
      InvalidIssuer;
    } else if (supporterIds |> List.mem(supporterId)) {
      DuplicateEndorsement;
    } else {
      Ok;
    };
  } {
  | Not_found => UnknownProcessId
  };

let validateAcceptance =
    (
      {processId, data}: EventTypes.acceptance('a),
      dataList: list((processId, 'a)),
      {processes, partnerIds},
      _issuerPubKey
    ) =>
  try {
    let {policy, supporterIds} = processes |> List.assoc(processId);
    if (data != (dataList |> List.assoc(processId))) {
      BadData;
    } else if (Policy.fulfilled(
                 ~eligable=partnerIds,
                 ~endorsed=supporterIds,
                 policy
               )
               == false) {
      PolicyNotFulfilled;
    } else {
      Ok;
    };
  } {
  | Not_found => UnknownProcessId
  };

let validateEvent =
  fun
  | VentureCreated(_) => ((_, _) => Ok)
  | PartnerProposed(proposal) =>
    validateProposal(proposal, Partner.processName)
  | PartnerLabelProposed(proposal) =>
    validateProposal(proposal, PartnerLabel.processName)
  | ContributionProposed(proposal) =>
    validateProposal(proposal, Contribution.processName)
  | PartnerEndorsed(endorsement) => validateEndorsement(endorsement)
  | PartnerLabelEndorsed(endorsement) => validateEndorsement(endorsement)
  | ContributionEndorsed(endorsement) => validateEndorsement(endorsement)
  | PartnerAccepted(acceptance) => (
      state => validateAcceptance(acceptance, state.partnerData, state)
    )
  | PartnerLabelAccepted(acceptance) => (
      state => validateAcceptance(acceptance, state.partnerLabelData, state)
    )
  | ContributionAccepted(acceptance) => (
      state => validateAcceptance(acceptance, state.contributionData, state)
    );

let validate = (state, {event, issuerPubKey}: EventLog.item) =>
  switch (
    event,
    Event.isSystemEvent(event),
    state.partnerPubKeys |> List.mem_assoc(issuerPubKey)
  ) {
  | (VentureCreated(_), _, _) => Ok
  | (_, false, false) => InvalidIssuer
  | (_, true, _) when issuerPubKey != state.systemPubKey => InvalidIssuer
  | _ => validateEvent(event, state, issuerPubKey)
  };
