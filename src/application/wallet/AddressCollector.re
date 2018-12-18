open Belt;

open Event;

type t = {
  network: Network.t,
  keyChains: AccountKeyChain.Collection.t,
  exposedAddresses: Map.String.t(Address.t),
};

let make = () => {
  network: Regtest,
  keyChains: AccountKeyChain.Collection.empty,
  exposedAddresses: Map.String.empty,
};

let apply = (event, state) =>
  switch (event) {
  | VentureCreated({network}) => {...state, network}
  | AccountKeyChainIdentified({keyChain}) => {
      ...state,
      keyChains: state.keyChains |> AccountKeyChain.Collection.add(keyChain),
    }
  | IncomeAddressExposed({address: {displayAddress} as address}) => {
      ...state,
      exposedAddresses:
        state.exposedAddresses->(Map.String.set(displayAddress, address)),
    }
  | _ => state
  };
