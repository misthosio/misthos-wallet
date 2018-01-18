type partner = {
  blockstackId: string,
  address: string,
  pubKey: string
};

type t = {partners: list(partner)};

let make = () => {partners: []};

let apply = (event: Event.t, state) =>
  switch event {
  | DealCreated({creatorId, creatorPubKey}) => {
      ...state,
      partners: [
        {
          blockstackId: creatorId,
          pubKey: creatorPubKey,
          address: Utils.addressFromPublicKey(creatorPubKey)
        }
      ]
    }
  /* | partnerAdded(event) => { */
  /*     partnerAddresses: [ */
  /*       Utils.addressFromPublicKey(event.pubKey), */
  /*       ...state.partnerAddresses */
  /*     ] */
  /*   } */
  | _ => state
  };

type validation =
  | Ok;

let validate = (_, _) => Ok;
