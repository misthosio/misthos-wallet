type t = {
  partnerIds: list(string),
  partnerAddresses: list(string),
  partnerPubKeys: list(string)
};

let make = () => {partnerIds: [], partnerAddresses: [], partnerPubKeys: []};

let apply = (event: Event.t, state) =>
  switch event {
  | DealCreated({creatorId, creatorPubKey}) => {
      ...state,
      partnerIds: [creatorId, ...state.partnerIds],
      partnerAddresses: [
        Utils.addressFromPublicKey(creatorPubKey),
        ...state.partnerAddresses
      ],
      partnerPubKeys: [creatorPubKey, ...state.partnerPubKeys]
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

let validate = (state, {issuerPubKey}: EventLog.item) =>
  if (state.partnerPubKeys |> List.mem(issuerPubKey)) {
    Ok;
  } else {
    InvalidIssuer;
  };
