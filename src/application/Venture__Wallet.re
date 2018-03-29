open WalletTypes;

open Event;

type t = {
  accountKeyChains:
    list((accountIdx, list((accountKeyChainIdx, AccountKeyChain.t))))
};

let make = () => {accountKeyChains: []};

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
      ...state,
      accountKeyChains: [
        (accountIndex, [(keyChainIndex, keyChain), ...accountKeyChains]),
        ...state.accountKeyChains |> List.remove_assoc(accountIndex)
      ]
    };
  | _ => state
  };

let exposeNextIncomeAddress = (accountIndex, {accountKeyChains}) => {
  let addressIndex = AddressIndex.first;
  let (keyChainIndex, keyChain) =
    accountKeyChains
    |> List.assoc(accountIndex)
    |> List.sort((a, b) => AccountKeyChainIndex.compare(a |> fst, b |> fst))
    |> List.rev
    |> List.hd;
  IncomeAddressExposed.make(
    ~accountIndex,
    ~keyChainIndex,
    ~addressIndex,
    ~address=(keyChain |> AccountKeyChain.getAddress(addressIndex)).address
  );
};
