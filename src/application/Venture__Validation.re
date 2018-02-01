open Event;

type prospect = {
  processId: string,
  supporterIds: list(string),
  policy: Policy.t
};

type contribution = {
  supporterIds: list(string),
  policy: Policy.t
};

type state = {
  ventureName: string,
  systemPubKey: string,
  metaPolicy: Policy.t,
  addPartnerPolicy: Policy.t,
  partnerIds: list(string),
  partnerAddresses: list(string),
  partnerPubKeys: list((string, string)),
  prospects: list((string, prospect)),
  acceptContributionPolicy: Policy.t,
  contributions: list((string, contribution))
};

let makeState = () => {
  ventureName: "",
  partnerIds: [],
  partnerAddresses: [],
  partnerPubKeys: [],
  systemPubKey: "",
  metaPolicy: Policy.absolute,
  addPartnerPolicy: Policy.absolute,
  prospects: [],
  acceptContributionPolicy: Policy.absolute,
  contributions: []
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
      addPartnerPolicy: metaPolicy,
      acceptContributionPolicy: metaPolicy
    }
  | ProspectSuggested({prospectId, processId, supporterId, policy}) => {
      ...state,
      prospects: [
        (prospectId, {processId, supporterIds: [supporterId], policy}),
        ...state.prospects
      ]
    }
  | PartnerAdded({blockstackId, pubKey}) => {
      ...state,
      partnerIds: [blockstackId, ...state.partnerIds],
      partnerAddresses: [
        Utils.addressFromPublicKey(pubKey),
        ...state.partnerAddresses
      ],
      partnerPubKeys: [(pubKey, blockstackId), ...state.partnerPubKeys],
      prospects:
        state.prospects
        |> List.filter(((prospectId, _)) => prospectId != blockstackId)
    }
  | ContributionSubmitted({processId, submitterId, policy}) => {
      ...state,
      contributions: [
        (processId, {policy, supporterIds: [submitterId]}),
        ...state.contributions
      ]
    }
  | ContributionApproved({processId, supporterId}) => {
      ...state,
      contributions:
        state.contributions
        |> List.map(((cProcess, c)) =>
             cProcess == processId ?
               (
                 cProcess,
                 {...c, supporterIds: [supporterId, ...c.supporterIds]}
               ) :
               (cProcess, c)
           )
    }
  | _ => state
  };

type result =
  | Ok
  | InvalidIssuer
  | UnknownProcessId
  | DuplicateApproval
  | PolicyMissmatch
  | PolicyNotFulfilled;

let validateProspectSuggested =
    (event: ProspectSuggested.t, _issuerPubKey, {addPartnerPolicy}) =>
  switch (addPartnerPolicy == event.policy) {
  | true => Ok
  | _ => PolicyMissmatch
  };

let validateProspectApproved =
    (
      {processId, prospectId, supporterId}: ProspectApproved.t,
      issuerPubKey,
      {prospects, partnerPubKeys}
    ) =>
  try {
    let prospect = prospects |> List.assoc(prospectId);
    if (prospect.processId != processId) {
      UnknownProcessId;
    } else if (partnerPubKeys |> List.assoc(issuerPubKey) != supporterId) {
      InvalidIssuer;
    } else if (prospect.supporterIds |> List.mem(supporterId)) {
      DuplicateApproval;
    } else {
      Ok;
    };
  } {
  | Not_found => UnknownProcessId
  };

let validatePartnerAdded =
    (
      {processId, blockstackId}: PartnerAdded.t,
      _issuerPubKey,
      {prospects, partnerIds}
    ) =>
  try {
    let prospect = prospects |> List.assoc(blockstackId);
    if (prospect.processId != processId) {
      UnknownProcessId;
    } else if (Policy.fulfilled(
                 ~eligable=partnerIds,
                 ~approved=prospect.supporterIds,
                 prospect.policy
               )
               == false) {
      PolicyNotFulfilled;
    } else {
      Ok;
    };
  } {
  | Not_found => UnknownProcessId
  };

let validateContributionSubmitted =
    (event: ContributionSubmitted.t, _issuerPubKey, {acceptContributionPolicy}) =>
  switch (acceptContributionPolicy == event.policy) {
  | true => Ok
  | _ => PolicyMissmatch
  };

let validateContributionApproved =
    (
      {processId, supporterId}: ContributionApproved.t,
      issuerPubKey,
      {contributions, partnerPubKeys}
    ) =>
  try {
    let contribution = contributions |> List.assoc(processId);
    if (partnerPubKeys |> List.assoc(issuerPubKey) != supporterId) {
      InvalidIssuer;
    } else if (contribution.supporterIds |> List.mem(supporterId)) {
      DuplicateApproval;
    } else {
      Ok;
    };
  } {
  | Not_found => UnknownProcessId
  };

let validateContributionAccepted =
    (
      {processId}: ContributionAccepted.t,
      _issuerPubKey,
      {contributions, partnerIds}
    ) =>
  try {
    let contribution = contributions |> List.assoc(processId);
    if (Policy.fulfilled(
          ~eligable=partnerIds,
          ~approved=contribution.supporterIds,
          contribution.policy
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
  | ProspectSuggested(event) => validateProspectSuggested(event)
  | ProspectApproved(event) => validateProspectApproved(event)
  | PartnerAdded(event) => validatePartnerAdded(event)
  | ContributionSubmitted(event) => validateContributionSubmitted(event)
  | ContributionApproved(event) => validateContributionApproved(event)
  | ContributionAccepted(event) => validateContributionAccepted(event)
  | _ => ((_, _) => Ok);

let validate = (state, {event, issuerPubKey}: EventLog.item) =>
  switch (
    event,
    Event.isSystemEvent(event),
    state.partnerPubKeys |> List.mem_assoc(issuerPubKey)
  ) {
  | (VentureCreated(_), _, _) => Ok
  | (_, false, false) => InvalidIssuer
  | (_, true, _) when issuerPubKey != state.systemPubKey => InvalidIssuer
  | _ => validateEvent(event, issuerPubKey, state)
  };

let processIdForProspect = (prospectId, state) =>
  (state.prospects |> List.assoc(prospectId)).processId;
