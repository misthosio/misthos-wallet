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
  custodianData: list((processId, Custodian.Data.t)),
  partnerLabelData: list((processId, PartnerLabel.Data.t)),
  contributionData: list((processId, Contribution.Data.t)),
  partnerDistributionData: list((processId, PartnerDistribution.Data.t)),
  labelDistributionData: list((processId, LabelDistribution.Data.t)),
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
  custodianData: [],
  contributionData: [],
  partnerDistributionData: [],
  labelDistributionData: [],
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
      partnerPubKeys:
        [(creatorPubKey, creatorId), ...state.partnerPubKeys]
        |> List.sort_uniq((a, b) => UserId.compare(a |> snd, b |> snd)),
      systemPubKey: systemIssuer |> Utils.publicKeyFromKeyPair,
      metaPolicy,
      policies:
        [
          Partner.processName,
          Custodian.processName,
          PartnerLabel.processName,
          Contribution.processName
        ]
        |> List.map(n => (n, metaPolicy))
    }
  | PartnerProposed({processId, data} as proposal) => {
      ...addProcess(proposal, state),
      partnerData: [(processId, data), ...state.partnerData]
    }
  | CustodianProposed({processId, data} as proposal) => {
      ...addProcess(proposal, state),
      custodianData: [(processId, data), ...state.custodianData]
    }
  | PartnerLabelProposed({processId, data} as proposal) => {
      ...addProcess(proposal, state),
      partnerLabelData: [(processId, data), ...state.partnerLabelData]
    }
  | ContributionProposed({processId, data} as proposal) => {
      ...addProcess(proposal, state),
      contributionData: [(processId, data), ...state.contributionData]
    }
  | PartnerDistributionProposed({processId, data} as proposal) => {
      ...addProcess(proposal, state),
      partnerDistributionData: [
        (processId, data),
        ...state.partnerDistributionData
      ]
    }
  | LabelDistributionProposed({processId, data} as proposal) => {
      ...addProcess(proposal, state),
      labelDistributionData: [
        (processId, data),
        ...state.labelDistributionData
      ]
    }
  | PartnerEndorsed(endorsement) => endorseProcess(endorsement, state)
  | CustodianEndorsed(endorsement) => endorseProcess(endorsement, state)
  | PartnerLabelEndorsed(endorsement) => endorseProcess(endorsement, state)
  | ContributionEndorsed(endorsement) => endorseProcess(endorsement, state)
  | PartnerDistributionEndorsed(endorsement) =>
    endorseProcess(endorsement, state)
  | LabelDistributionEndorsed(endorsement) =>
    endorseProcess(endorsement, state)
  | PartnerAccepted({data}) => {
      ...state,
      partnerIds:
        [data.id, ...state.partnerIds] |> List.sort_uniq(UserId.compare),
      partnerAddresses: [
        Utils.addressFromPublicKey(data.pubKey),
        ...state.partnerAddresses
      ],
      partnerPubKeys:
        [(data.pubKey, data.id), ...state.partnerPubKeys]
        |> List.sort_uniq((a, b) => UserId.compare(a |> snd, b |> snd))
    }
  | CustodianAccepted(_)
  | PartnerLabelAccepted(_)
  | ContributionAccepted(_)
  | PartnerDistributionAccepted(_)
  | LabelDistributionAccepted(_) => state
  };

type result =
  | Ok
  | InvalidIssuer
  | UnknownProcessId
  | BadData
  | DuplicateEndorsement
  | PolicyMissmatch
  | PolicyNotFulfilled;

let defaultDataValidator = (_, _) => Ok;

let validateProposal =
    (
      ~validateData: ('a, state) => result=defaultDataValidator,
      processName,
      {policy, supporterId, data}: EventTypes.proposal('a),
      {policies, partnerPubKeys} as state,
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
    validateData(data, state);
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

let validateCustodianData = (data: Custodian.Data.t, {partnerIds}) =>
  partnerIds |> List.mem(data.partnerId) ? Ok : BadData;

let validatePartneLabelData = (data: PartnerLabel.Data.t, {partnerIds}) =>
  partnerIds |> List.mem(data.partnerId) ? Ok : BadData;

let validateEvent =
  fun
  | VentureCreated(_) => ((_, _) => Ok)
  | PartnerProposed(proposal) =>
    validateProposal(Partner.processName, proposal)
  | CustodianProposed(proposal) =>
    validateProposal(
      ~validateData=validateCustodianData,
      Custodian.processName,
      proposal
    )
  | PartnerLabelProposed(proposal) =>
    validateProposal(
      ~validateData=validatePartneLabelData,
      PartnerLabel.processName,
      proposal
    )
  | ContributionProposed(proposal) =>
    validateProposal(Contribution.processName, proposal)
  | PartnerDistributionProposed(proposal) =>
    validateProposal(PartnerDistribution.processName, proposal)
  | LabelDistributionProposed(proposal) =>
    validateProposal(LabelDistribution.processName, proposal)
  | PartnerEndorsed(endorsement) => validateEndorsement(endorsement)
  | CustodianEndorsed(endorsement) => validateEndorsement(endorsement)
  | PartnerLabelEndorsed(endorsement) => validateEndorsement(endorsement)
  | ContributionEndorsed(endorsement) => validateEndorsement(endorsement)
  | PartnerDistributionEndorsed(endorsement) =>
    validateEndorsement(endorsement)
  | LabelDistributionEndorsed(endorsement) => validateEndorsement(endorsement)
  | PartnerAccepted(acceptance) => (
      state => validateAcceptance(acceptance, state.partnerData, state)
    )
  | CustodianAccepted(acceptance) => (
      state => validateAcceptance(acceptance, state.custodianData, state)
    )
  | PartnerLabelAccepted(acceptance) => (
      state => validateAcceptance(acceptance, state.partnerLabelData, state)
    )
  | ContributionAccepted(acceptance) => (
      state => validateAcceptance(acceptance, state.contributionData, state)
    )
  | PartnerDistributionAccepted(acceptance) => (
      state =>
        validateAcceptance(acceptance, state.partnerDistributionData, state)
    )
  | LabelDistributionAccepted(acceptance) => (
      state =>
        validateAcceptance(acceptance, state.labelDistributionData, state)
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
