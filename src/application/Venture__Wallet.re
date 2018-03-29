open WalletTypes;

open Event;

type t = {
  accountKeyChains:
    list((accountIdx, list((accountKeyChainIdx, AccountKeyChain.t)))),
  nextCoordinates: list((accountIdx, AddressCoordinates.t)),
  exposedCoordinates: list((accountIdx, list(AddressCoordinates.t)))
};

let make = () => {
  accountKeyChains: [],
  nextCoordinates: [],
  exposedCoordinates: []
};

let apply = (event: Event.t, state) =>
  switch event {
  | AccountKeyChainUpdated(
      ({accountIdx, keyChainIdx, keyChain}: AccountKeyChainUpdated.t)
    ) =>
    let accountKeyChains =
      try (state.accountKeyChains |> List.assoc(accountIdx)) {
      | Not_found => []
      };
    {
      ...state,
      accountKeyChains: [
        (accountIdx, [(keyChainIdx, keyChain), ...accountKeyChains]),
        ...state.accountKeyChains |> List.remove_assoc(accountIdx)
      ],
      nextCoordinates: [
        (accountIdx, AddressCoordinates.first(accountIdx, keyChainIdx)),
        ...state.nextCoordinates |> List.remove_assoc(accountIdx)
      ]
    };
  | IncomeAddressExposed(({coordinates}: IncomeAddressExposed.t)) =>
    let accountIdx = coordinates |> AddressCoordinates.accountIdx;
    let previouslyExposed =
      try (state.exposedCoordinates |> List.assoc(accountIdx)) {
      | Not_found => []
      };
    {
      ...state,
      nextCoordinates: [
        (accountIdx, coordinates |> AddressCoordinates.next),
        ...state.nextCoordinates |> List.remove_assoc(accountIdx)
      ],
      exposedCoordinates: [
        (accountIdx, [coordinates, ...previouslyExposed]),
        ...state.exposedCoordinates
      ]
    };
  | _ => state
  };

let exposeNextIncomeAddress = (accountIdx, {nextCoordinates, accountKeyChains}) => {
  let coordinates = nextCoordinates |> List.assoc(accountIdx);
  let address = accountKeyChains |> AccountKeyChain.find(coordinates);
  IncomeAddressExposed.make(~coordinates, ~address=address.address);
};

let preparePayoutTx =
    (accountIdx, destinations, fee, {exposedCoordinates, accountKeyChains}) => {
  let addressPairs =
    exposedCoordinates
    |> List.assoc(accountIdx)
    |> List.map(c => (c, accountKeyChains |> AccountKeyChain.find(c)));
  let addresses = addressPairs |> List.map(p => snd(p));
  ();
  /* find all potential input addresses */
  /*   build transaction */
  /*   get utxos */
};
