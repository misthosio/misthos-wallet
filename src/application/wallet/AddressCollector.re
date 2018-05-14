open Belt;

open Event;

type t = {
  network: Network.t,
  keyChains: AccountKeyChain.Collection.t,
  exposedAddresses: Map.String.t(Address.t),
};

let make = () => {
  network: Regtest,
  keyChains: AccountKeyChain.Collection.make(),
  exposedAddresses: Map.String.empty,
};

let apply = (event, state) =>
  switch (event) {
  | VentureCreated({network}) => {...state, network}
  | AccountKeyChainIdentified({keyChain}) => {
      ...state,
      keyChains: state.keyChains |> AccountKeyChain.Collection.add(keyChain),
    }
  | IncomeAddressExposed({address, coordinates}) => {
      ...state,
      exposedAddresses:
        state.exposedAddresses
        |. Map.String.set(
             address,
             state.keyChains |> Address.find(coordinates),
           ),
    }
  | _ => state
  };
