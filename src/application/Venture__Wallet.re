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
      ({accountIdx, keyChainIdx, keyChain}: AccountKeyChainUpdated.t)
    ) =>
    let accountKeyChains =
      try (state.accountKeyChains |> List.assoc(accountIdx)) {
      | Not_found => []
      };
    {
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
    {
      ...state,
      nextCoordinates: [
        (accountIdx, coordinates |> AddressCoordinates.next),
        ...state.nextCoordinates |> List.remove_assoc(accountIdx)
      ]
    };
  | _ => state
  };

let exposeNextIncomeAddress = (accountIdx, {nextCoordinates, accountKeyChains}) => {
  let coordinates = nextCoordinates |> List.assoc(accountIdx);
  let address = accountKeyChains |> AccountKeyChain.find(coordinates);
  IncomeAddressExposed.make(~coordinates, ~address=address.address);
};

let preparePayoutTransaction = (destinations, fee, accountIdx) => ();
