open PrimitiveTypes;

open WalletTypes;

open Event;

type t = {
  ventureId,
  network: Network.t,
  payoutPolicy: Policy.t,
  accountKeyChains:
    list((accountIdx, list((accountKeyChainIdx, AccountKeyChain.t)))),
  nextCoordinates: list((accountIdx, AccountKeyChain.Address.Coordinates.t)),
  nextChangeCoordinates:
    list((accountIdx, AccountKeyChain.Address.Coordinates.t)),
  exposedCoordinates:
    list((accountIdx, list(AccountKeyChain.Address.Coordinates.t))),
  reservedInputs: list(Network.txInput),
  payoutProcesses: list((ProcessId.t, PayoutTransaction.t))
};

type balance = {
  total: BTC.t,
  reserved: BTC.t
};

let make = () => {
  network: Network.Testnet,
  ventureId: VentureId.fromString(""),
  payoutPolicy: Policy.absolute,
  accountKeyChains: [],
  nextCoordinates: [],
  nextChangeCoordinates: [],
  exposedCoordinates: [],
  reservedInputs: [],
  payoutProcesses: []
};

let apply = (event: Event.t, state) =>
  switch event {
  | VentureCreated({ventureId, metaPolicy, network}) => {
      ...state,
      network,
      ventureId,
      payoutPolicy: metaPolicy
    }
  | AccountCreationAccepted({data}) => {
      ...state,
      exposedCoordinates: [(data.accountIdx, []), ...state.exposedCoordinates],
      accountKeyChains: [(data.accountIdx, []), ...state.accountKeyChains]
    }
  | AccountKeyChainUpdated(({keyChain}: AccountKeyChainUpdated.t)) => {
      ...state,
      accountKeyChains: [
        (
          keyChain.accountIdx,
          [
            (keyChain.keyChainIdx, keyChain),
            ...state.accountKeyChains |> List.assoc(keyChain.accountIdx)
          ]
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
    }
  | IncomeAddressExposed(({coordinates}: IncomeAddressExposed.t)) =>
    let accountIdx =
      coordinates |> AccountKeyChain.Address.Coordinates.accountIdx;
    {
      ...state,
      nextCoordinates: [
        (accountIdx, coordinates |> AccountKeyChain.Address.Coordinates.next),
        ...state.nextCoordinates |> List.remove_assoc(accountIdx)
      ],
      exposedCoordinates: [
        (
          accountIdx,
          [coordinates, ...state.exposedCoordinates |> List.assoc(accountIdx)]
        ),
        ...state.exposedCoordinates
      ]
    };
  | PayoutProposed({data, processId}) => {
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
      ],
      payoutProcesses: [(processId, data.payoutTx), ...state.payoutProcesses]
    }
  | PayoutBroadcast({processId}) => {
      ...state,
      reservedInputs:
        state.reservedInputs
        |> List.filter((input: Network.txInput) =>
             (state.payoutProcesses |> List.assoc(processId)).usedInputs
             |> List.map(snd)
             |>
             List.exists((i: Network.txInput) =>
               input.txId == i.txId && input.txOutputN == i.txOutputN
             ) == false
           )
    }
  | PayoutBroadcastFailed({processId}) => {
      ...state,
      reservedInputs:
        state.reservedInputs
        |> List.filter((input: Network.txInput) =>
             (state.payoutProcesses |> List.assoc(processId)).usedInputs
             |> List.map(snd)
             |>
             List.exists((i: Network.txInput) =>
               input.txId == i.txId && input.txOutputN == i.txOutputN
             ) == false
           )
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
      {userId, masterKeyChain, network}: Session.Data.t,
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
  let coordinates = exposedCoordinates |> List.assoc(accountIdx);
  let nextChangeCoordinates = nextChangeCoordinates |> List.assoc(accountIdx);
  let currentKeyChainIdx = nextChangeCoordinates |> Coordinates.keyChainIdx;
  Js.Promise.(
    accountKeyChains
    |> Network.transactionInputs(network, coordinates)
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
               ~network
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
               ~network
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
    (
      accountIdx,
      {exposedCoordinates, accountKeyChains, reservedInputs, network}
    ) => {
  let coordinates = exposedCoordinates |> List.assoc(accountIdx);
  Js.Promise.(
    accountKeyChains
    |> Network.transactionInputs(network, coordinates)
    |> then_(inputs =>
         inputs
         |> List.fold_left(
              ({total, reserved}, input: Network.txInput) => {
                total: total |> BTC.plus(input.value),
                reserved:
                  reserved
                  |> BTC.plus(
                       reservedInputs
                       |> List.exists((reservedIn: Network.txInput) =>
                            reservedIn.txId == input.txId
                            && reservedIn.txOutputN == input.txOutputN
                          ) ?
                         input.value : BTC.zero
                     )
              },
              {total: BTC.zero, reserved: BTC.zero}
            )
         |> resolve
       )
  );
};
