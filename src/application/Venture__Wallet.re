open PrimitiveTypes;

open WalletTypes;

open Event;

let faucetAddress = "2N8hwP1WmJrFF5QWABn38y63uYLhnJYJYTF";

type t = {
  ventureId,
  network: Network.t,
  payoutPolicy: Policy.t,
  accountKeyChains: AccountKeyChain.Collection.t,
  exposedCoordinates: list(Address.Coordinates.t),
  reservedInputs: list(Network.txInput),
  payoutProcesses: list((ProcessId.t, PayoutTransaction.t)),
};

let make = () => {
  network: Network.Testnet,
  ventureId: VentureId.fromString(""),
  payoutPolicy: Policy.unanimous,
  accountKeyChains: [],
  exposedCoordinates: [],
  reservedInputs: [],
  payoutProcesses: [],
};

let apply = (event: Event.t, state) =>
  switch (event) {
  | VentureCreated({ventureId, metaPolicy, network}) => {
      ...state,
      network,
      ventureId,
      payoutPolicy: metaPolicy,
    }
  | AccountKeyChainUpdated(({keyChain}: AccountKeyChainUpdated.t)) => {
      ...state,
      accountKeyChains:
        state.accountKeyChains |> AccountKeyChain.Collection.add(keyChain),
    }
  | IncomeAddressExposed(({coordinates}: IncomeAddressExposed.t)) => {
      ...state,
      exposedCoordinates: [coordinates, ...state.exposedCoordinates],
    }
  | PayoutProposed({data, processId}) => {
      ...state,
      reservedInputs:
        state.reservedInputs
        |> List.rev_append(data.payoutTx.usedInputs |> List.map(snd)),
      exposedCoordinates:
        switch (data.changeAddressCoordinates) {
        | None => state.exposedCoordinates
        | Some(coordinates) => [coordinates, ...state.exposedCoordinates]
        },
      payoutProcesses: [
        (processId, data.payoutTx),
        ...state.payoutProcesses,
      ],
    }
  | PayoutBroadcast({processId}) =>
    let payoutTx = state.payoutProcesses |> List.assoc(processId);
    {
      ...state,
      reservedInputs:
        state.reservedInputs
        |> List.filter((input: Network.txInput) =>
             payoutTx.usedInputs
             |> List.map(snd)
             |>
             List.exists((i: Network.txInput) =>
               input.txId == i.txId && input.txOutputN == i.txOutputN
             ) == false
           ),
    };
  | PayoutBroadcastFailed({processId}) =>
    let payoutTx = state.payoutProcesses |> List.assoc(processId);
    {
      ...state,
      reservedInputs:
        state.reservedInputs
        |> List.filter((input: Network.txInput) =>
             payoutTx.usedInputs
             |> List.map(snd)
             |>
             List.exists((i: Network.txInput) =>
               input.txId == i.txId && input.txOutputN == i.txOutputN
             ) == false
           ),
    };
  | _ => state
  };

let exposeNextIncomeAddress =
    (userId, accountIdx, {exposedCoordinates, accountKeyChains}) => {
  let accountKeyChain =
    accountKeyChains |> AccountKeyChain.Collection.latest(accountIdx);
  let coordinates =
    Address.Coordinates.nextExternal(
      userId,
      exposedCoordinates,
      accountKeyChain,
    );
  IncomeAddressExposed.make(
    ~coordinates,
    ~address=Address.make(coordinates, accountKeyChain).address,
  );
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
        exposedCoordinates,
        accountKeyChains,
        reservedInputs,
      },
    ) => {
  open Address;
  let accountKeyChain: AccountKeyChain.t =
    accountKeyChains |> AccountKeyChain.Collection.latest(accountIdx);
  let currentKeyChainIdx = accountKeyChain.keyChainIdx;
  let coordinates =
    exposedCoordinates |> Coordinates.allForAccount(accountIdx);
  let nextChangeCoordinates =
    Coordinates.nextInternal(userId, coordinates, accountKeyChain);
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
           Address.find(nextChangeCoordinates, accountKeyChains);
         let payoutTx =
           PayoutTransaction.build(
             ~mandatoryInputs=oldInputs,
             ~allInputs=inputs,
             ~destinations,
             ~satsPerByte,
             ~changeAddress,
             ~network,
           );
         let changeAddressCoordinates =
           payoutTx.changeAddress
           |> Utils.mapOption((_) => nextChangeCoordinates);
         let payoutTx =
           switch (
             PayoutTransaction.signPayout(
               ~ventureId,
               ~userId,
               ~masterKeyChain,
               ~accountKeyChains,
               ~payoutTx,
               ~network,
             )
           ) {
           | Signed(payout) => payout
           | NotSigned => payoutTx
           };
         Event.Payout.(
           Proposed.make(
             ~supporterId=userId,
             ~policy=payoutPolicy,
             Data.{accountIdx, payoutTx, changeAddressCoordinates},
           )
         )
         |> resolve;
       })
  );
};
