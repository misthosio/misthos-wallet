open Event;

type prospect = {
  processId: string,
  supporterIds: list(string),
  policy: Policy.t
};

type state = {
  ventureName: string,
  partnerIds: list(string),
  partnerAddresses: list(string),
  partnerPubKeys: list((string, string)),
  systemPubKey: string,
  metaPolicy: Policy.t,
  addPartnerPolicy: Policy.t,
  prospects: list((string, prospect))
};

let makeState = () => {
  ventureName: "",
  partnerIds: [],
  partnerAddresses: [],
  partnerPubKeys: [],
  systemPubKey: "",
  metaPolicy: Policy.absolute,
  addPartnerPolicy: Policy.absolute,
  prospects: []
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
      addPartnerPolicy: metaPolicy
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
  | _ => state
  };

type result =
  | Ok
  | InvalidIssuer
  | UnknownProcessId
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

let validateEvent =
  fun
  | ProspectSuggested(event) => validateProspectSuggested(event)
  | ProspectApproved(event) => validateProspectApproved(event)
  | PartnerAdded(event) => validatePartnerAdded(event)
  | _ => ((_, _) => Ok);

let validate = (state, {event, issuerPubKey}: EventLog.item) =>
  if (Event.isSystemEvent(event) && issuerPubKey != state.systemPubKey) {
    InvalidIssuer;
  } else if (state.partnerPubKeys |> List.mem_assoc(issuerPubKey) == false) {
    InvalidIssuer;
  } else {
    validateEvent(event, issuerPubKey, state);
  };

let processIdForProspect = (prospectId, state) =>
  (state.prospects |> List.assoc(prospectId)).processId;
