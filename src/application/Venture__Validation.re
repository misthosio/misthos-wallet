open Event;

type state = {
  partnerIds: list(string),
  partnerAddresses: list(string),
  partnerPubKeys: list(string),
  systemPubKey: string,
  metaPolicy: Policy.t
};

let makeState = () => {
  partnerIds: [],
  partnerAddresses: [],
  partnerPubKeys: [],
  systemPubKey: "",
  metaPolicy: Policy.absolute
};

let apply = (event: Event.t, state) =>
  switch event {
  | VentureCreated({creatorId, creatorPubKey, metaPolicy, systemIssuer}) => {
      ...state,
      partnerIds: [creatorId, ...state.partnerIds],
      partnerAddresses: [
        Utils.addressFromPublicKey(creatorPubKey),
        ...state.partnerAddresses
      ],
      partnerPubKeys: [creatorPubKey, ...state.partnerPubKeys],
      systemPubKey: systemIssuer |> Utils.publicKeyFromKeyPair,
      metaPolicy
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
  | InvalidIssuer;

let validateProspectSuggested = (event, state) => Ok;

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
