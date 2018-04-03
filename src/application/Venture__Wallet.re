open WalletTypes;

open Event;

open Bitcoin;

type t = {
  accountKeyChains:
    list((accountIdx, list((accountKeyChainIdx, AccountKeyChain.t)))),
  nextCoordinates: list((accountIdx, AccountKeyChain.Address.Coordinates.t)),
  exposedCoordinates:
    list((accountIdx, list(AccountKeyChain.Address.Coordinates.t)))
};

let make = () => {
  accountKeyChains: [],
  nextCoordinates: [],
  exposedCoordinates: []
};

let apply = (event: Event.t, state) =>
  switch event {
  | AccountKeyChainUpdated(({keyChain}: AccountKeyChainUpdated.t)) =>
    let accountKeyChains =
      try (state.accountKeyChains |> List.assoc(keyChain.accountIdx)) {
      | Not_found => []
      };
    {
      ...state,
      accountKeyChains: [
        (
          keyChain.accountIdx,
          [(keyChain.keyChainIdx, keyChain), ...accountKeyChains]
        ),
        ...state.accountKeyChains |> List.remove_assoc(keyChain.accountIdx)
      ],
      nextCoordinates: [
        (
          keyChain.accountIdx,
          AccountKeyChain.Address.Coordinates.firstExternal(keyChain)
        ),
        ...state.nextCoordinates |> List.remove_assoc(keyChain.accountIdx)
      ]
    };
  | IncomeAddressExposed(({coordinates}: IncomeAddressExposed.t)) =>
    let accountIdx =
      coordinates |> AccountKeyChain.Address.Coordinates.accountIdx;
    let previouslyExposed =
      try (state.exposedCoordinates |> List.assoc(accountIdx)) {
      | Not_found => []
      };
    {
      ...state,
      nextCoordinates: [
        (accountIdx, coordinates |> AccountKeyChain.Address.Coordinates.next),
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
  module Network = Network.Regtest;
  let addressPairs =
    exposedCoordinates
    |> List.assoc(accountIdx)
    |> List.map(c => (c, accountKeyChains |> AccountKeyChain.find(c)));
  let addresses = addressPairs |> List.map(p => snd(p));
  let addressLookup = addressPairs |> List.map((c, a) => (a.address, c));
  ();
  /* get utxos */
  /* Js.Promise.( */
  /*   Network.getUTXOs(addresses) */
  /*   |> then_((_) => { */
  /*        let txB = TxBuilder.createWithNetwork(Network.network); */
  /*        resolve(); */
  /*      }) */
  /* ); */
  /* select from utxos */
  /* for now use all utxos */
  /* estimate size */
  /*   build transaction */
};
