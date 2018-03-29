open WalletTypes;

open Event;

type t = {
  accountKeyChains:
    list((accountIdx, list((accountKeyChainIdx, AccountKeyChain.t)))),
  nextCoordinates: list((accountIdx, AddressCoordinates.t))
};

let make = () => {accountKeyChains: [], nextCoordinates: []};

let apply = (event: Event.t, state) =>
  switch event {
  | AccountKeyChainUpdated(
      ({accountIndex, keyChainIndex, keyChain}: AccountKeyChainUpdated.t)
    ) =>
    let accountKeyChains =
      try (state.accountKeyChains |> List.assoc(accountIndex)) {
      | Not_found => []
      };
    {
      accountKeyChains: [
        (accountIndex, [(keyChainIndex, keyChain), ...accountKeyChains]),
        ...state.accountKeyChains |> List.remove_assoc(accountIndex)
      ],
      nextCoordinates: [
        (accountIndex, AddressCoordinates.first(accountIndex, keyChainIndex)),
        ...state.nextCoordinates |> List.remove_assoc(accountIndex)
      ]
    };
  | IncomeAddressExposed(({coordinates}: IncomeAddressExposed.t)) =>
    let accountIdx = coordinates |> AddressCoordinates.accountIdx;
    {
      ...state,
      nextCoordinates: [
        (accountIdx, coordinates |> AddressCoordinates.next),
        ...state.nextCoordinates |> List.remove_assoc(accountIdx)
      ]
    };
  | _ => state
  };

let exposeNextIncomeAddress =
    (accountIndex, {nextCoordinates, accountKeyChains}) => {
  let coordinates = nextCoordinates |> List.assoc(accountIndex);
  let address = accountKeyChains |> AccountKeyChain.find(coordinates);
  IncomeAddressExposed.make(~coordinates, ~address=address.address);
};
