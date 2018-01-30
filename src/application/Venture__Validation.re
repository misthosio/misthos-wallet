open Event;

type state = {
  ventureName: string,
  partnerIds: list(string),
  partnerAddresses: list(string),
  partnerPubKeys: list(string),
  systemPubKey: string,
  metaPolicy: Policy.t,
  addPartnerPolicy: Policy.t,
  prospectIds: list((string, string))
};

let makeState = () => {
  ventureName: "",
  partnerIds: [],
  partnerAddresses: [],
  partnerPubKeys: [],
  systemPubKey: "",
  metaPolicy: Policy.absolute,
  addPartnerPolicy: Policy.absolute,
  prospectIds: []
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
      partnerPubKeys: [creatorPubKey, ...state.partnerPubKeys],
      systemPubKey: systemIssuer |> Utils.publicKeyFromKeyPair,
      metaPolicy,
      addPartnerPolicy: metaPolicy
    }
  | ProspectSuggested({prospectId, processId}) => {
      ...state,
      prospectIds: [(prospectId, processId), ...state.prospectIds]
    }
  | PartnerAdded({blockstackId, pubKey}) => {
      ...state,
      partnerIds: [blockstackId, ...state.partnerIds],
      partnerAddresses: [
        Utils.addressFromPublicKey(pubKey),
        ...state.partnerAddresses
      ],
      partnerPubKeys: [pubKey, ...state.partnerPubKeys]
    }
  | _ => state
  };

type result =
  | Ok
  | InvalidIssuer
  | PartnerApprovalPolicyConflict(ProspectSuggested.t, Policy.t);

let validateProspectSuggested = (event: ProspectSuggested.t, state) =>
  switch (state.addPartnerPolicy == event.policy) {
  | true => Ok
  | _ => PartnerApprovalPolicyConflict(event, state.addPartnerPolicy)
  };

let validateEvent =
  fun
  | ProspectSuggested(event) => validateProspectSuggested(event)
  | _ => ((_) => Ok);

let validate = (state, {event, issuerPubKey}: EventLog.item) =>
  if (Event.isSystemEvent(event) && issuerPubKey != state.systemPubKey) {
    InvalidIssuer;
  } else if (state.partnerPubKeys |> List.mem(issuerPubKey) == false) {
    InvalidIssuer;
  } else {
    validateEvent(event, state);
  };

let processIdForProspect = (prospectId, state) =>
  state.prospectIds |> List.assoc(prospectId);
