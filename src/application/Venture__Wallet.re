open PrimitiveTypes;

open WalletTypes;

open Event;

type t = {
  ventureId,
  payoutPolicy: Policy.t,
  accountKeyChains:
    list((accountIdx, list((accountKeyChainIdx, AccountKeyChain.t)))),
  nextCoordinates: list((accountIdx, AccountKeyChain.Address.Coordinates.t)),
  nextChangeCoordinates:
    list((accountIdx, AccountKeyChain.Address.Coordinates.t)),
  exposedCoordinates:
    list((accountIdx, list(AccountKeyChain.Address.Coordinates.t)))
};

let make = () => {
  ventureId: VentureId.fromString(""),
  payoutPolicy: Policy.absolute,
  accountKeyChains: [],
  nextCoordinates: [],
  nextChangeCoordinates: [],
  exposedCoordinates: []
};

let apply = (event: Event.t, state) =>
  switch event {
  | VentureCreated({ventureId, metaPolicy}) => {
      ...state,
      ventureId,
      payoutPolicy: metaPolicy
    }
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
      ],
      nextChangeCoordinates: [
        (
          keyChain.accountIdx,
          AccountKeyChain.Address.Coordinates.firstInternal(keyChain)
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
    (
      session: Session.Data.t,
      accountIdx,
      destinations,
      satsPerByte,
      {
        ventureId,
        payoutPolicy,
        nextChangeCoordinates,
        exposedCoordinates,
        accountKeyChains
      }
    ) => {
  open AccountKeyChain.Address;
  module UseNetwork = Network.Regtest;
  let coordinates = exposedCoordinates |> List.assoc(accountIdx);
  let nextChangeCoordinates = nextChangeCoordinates |> List.assoc(accountIdx);
  let currentKeyChainIdx = nextChangeCoordinates |> Coordinates.keyChainIdx;
  Js.Promise.(
    accountKeyChains
    |> UseNetwork.getTransactionInputs(coordinates)
    |> then_(inputs => {
         let oldInputs =
           inputs
           |> List.find_all((i: Network.txInput) =>
                i.coordinates
                |> Coordinates.keyChainIdx
                |> AccountKeyChainIndex.neq(currentKeyChainIdx)
              );
         let changeAddress =
           AccountKeyChain.find(nextChangeCoordinates, accountKeyChains);
         let (payoutTx, changeAddressCoordinates) =
           switch (
             PayoutTransaction.build(
               ~mandatoryInputs=oldInputs,
               ~allInputs=inputs,
               ~destinations,
               ~satsPerByte,
               ~changeAddress,
               ~network=UseNetwork.network
             )
           ) {
           | WithChangeAddress(payout) => (payout, Some(nextChangeCoordinates))
           | WithoutChangeAddress(payout) => (payout, None)
           };
         let payoutTx =
           switch (
             PayoutTransaction.signPayout(
               ~ventureId,
               ~session,
               ~accountKeyChains,
               ~payoutTx,
               ~network=UseNetwork.network
             )
           ) {
           | Signed(payout) => payout
           | NotSigned => payoutTx
           };
         Event.Payout.(
           Proposal.make(
             ~supporterId=session.userId,
             ~policy=payoutPolicy,
             Data.{accountIdx, payoutTx, changeAddressCoordinates}
           )
         )
         |> resolve;
       })
  );
};
