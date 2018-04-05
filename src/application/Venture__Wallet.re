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
    list((accountIdx, list(AccountKeyChain.Address.Coordinates.t))),
  reservedInputs: list(Network.txInput)
};

let make = () => {
  ventureId: VentureId.fromString(""),
  payoutPolicy: Policy.absolute,
  accountKeyChains: [],
  nextCoordinates: [],
  nextChangeCoordinates: [],
  exposedCoordinates: [],
  reservedInputs: []
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
  | PayoutProposed({data}) => {
      ...state,
      reservedInputs:
        state.reservedInputs
        |> List.rev_append(data.payoutTx.usedInputs |> List.map(snd)),
      exposedCoordinates:
        switch data.changeAddressCoordinates {
        | None => state.exposedCoordinates
        | Some(coordinates) => [
            (
              data.accountIdx,
              [
                coordinates,
                ...state.exposedCoordinates |> List.assoc(data.accountIdx)
              ]
            ),
            ...state.exposedCoordinates |> List.remove_assoc(data.accountIdx)
          ]
        },
      nextChangeCoordinates: [
        (
          data.accountIdx,
          state.nextChangeCoordinates
          |> List.assoc(data.accountIdx)
          |> AccountKeyChain.Address.Coordinates.next
        ),
        ...state.nextCoordinates |> List.remove_assoc(data.accountIdx)
      ]
    }
  | _ => state
  };

let exposeNextIncomeAddress = (accountIdx, {nextCoordinates, accountKeyChains}) => {
  let coordinates = nextCoordinates |> List.assoc(accountIdx);
  let address = accountKeyChains |> AccountKeyChain.find(coordinates);
  IncomeAddressExposed.make(~coordinates, ~address=address.address);
};

let preparePayoutTx =
    (
      {userId, masterKeyChain}: Session.Data.t,
      accountIdx,
      destinations,
      satsPerByte,
      {
        ventureId,
        payoutPolicy,
        nextChangeCoordinates,
        exposedCoordinates,
        accountKeyChains,
        reservedInputs
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
         let inputs =
           inputs
           |> List.filter((input: Network.txInput) =>
                reservedInputs
                |>
                List.exists((reservedIn: Network.txInput) =>
                  reservedIn.txId == input.txId
                  && reservedIn.txOutputN == input.txOutputN
                ) == false
              );
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
               ~userId,
               ~masterKeyChain,
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
             ~supporterId=userId,
             ~policy=payoutPolicy,
             Data.{accountIdx, payoutTx, changeAddressCoordinates}
           )
         )
         |> resolve;
       })
  );
};

let balance =
    (accountIdx, {exposedCoordinates, accountKeyChains, reservedInputs}) => {
  module UseNetwork = Network.Regtest;
  let coordinates = exposedCoordinates |> List.assoc(accountIdx);
  Js.Promise.(
    accountKeyChains
    |> UseNetwork.getTransactionInputs(coordinates)
    |> then_(inputs =>
         inputs
         |> List.fold_left(
              ((total, reserved), input: Network.txInput) => (
                total |> BTC.plus(input.value),
                reserved
                |> BTC.plus(
                     reservedInputs
                     |> List.exists((reservedIn: Network.txInput) =>
                          reservedIn.txId == input.txId
                          && reservedIn.txOutputN == input.txOutputN
                        ) ?
                       input.value : BTC.zero
                   )
              ),
              (BTC.zero, BTC.zero)
            )
         |> resolve
       )
  );
};
