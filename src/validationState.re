type t = {
  partnerIds: list(string),
  partnerAddresses: list(string),
  partnerPubKeys: list(string),
  systemPubKey: string
};

let make = () => {
  partnerIds: [],
  partnerAddresses: [],
  partnerPubKeys: [],
  systemPubKey: ""
};

let apply = (event: Event.t, state) =>
  switch event {
  | VentureCreated({creatorId, creatorPubKey, systemIssuer}) => {
      ...state,
      partnerIds: [creatorId, ...state.partnerIds],
      partnerAddresses: [
        Utils.addressFromPublicKey(creatorPubKey),
        ...state.partnerAddresses
      ],
      partnerPubKeys: [creatorPubKey, ...state.partnerPubKeys],
      systemPubKey: systemIssuer |> Utils.publicKeyFromKeyPair
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

type validation =
  | Ok
  | InvalidIssuer;

let validate = (state, {event, issuerPubKey}: EventLog.item) =>
  if (Event.isSystemEvent(event) && issuerPubKey == state.systemPubKey) {
    Ok;
  } else if (state.partnerPubKeys |> List.mem(issuerPubKey)) {
    Ok;
  } else {
    InvalidIssuer;
  };
