open PrimitiveTypes;

open WalletTypes;

open Event;

type balance = {
  income: BTC.t,
  spent: BTC.t,
  reserved: BTC.t,
};

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
    list(
      (accountIdx, list((AccountKeyChain.Address.Coordinates.t, bool))),
    ),
  reservedInputs: list(Network.txInput),
  payoutProcesses: list((ProcessId.t, (accountIdx, PayoutTransaction.t))),
  knownIncomeTxIds: list(string),
  balance: list((accountIdx, balance)),
};

let make = () => {
  network: Network.Testnet,
  ventureId: VentureId.fromString(""),
  payoutPolicy: Policy.unanimous,
  accountKeyChains: [],
  nextCoordinates: [],
  nextChangeCoordinates: [],
  exposedCoordinates: [],
  reservedInputs: [],
  payoutProcesses: [],
  knownIncomeTxIds: [],
  balance: [],
};

let getExposedAddresses =
    (~includeChangeAddresses=false, {exposedCoordinates, accountKeyChains}) =>
  exposedCoordinates
  |> List.map(((_idx, coordinates)) =>
       coordinates
       |> List.filter(c => snd(c) || includeChangeAddresses)
       |> List.map(fst)
       |> List.map(c => accountKeyChains |> AccountKeyChain.find(c))
     )
  |> List.flatten
  |> List.map((a: AccountKeyChain.Address.t) => a.address);

let getAccountIndexOfAddress =
    (address, {accountKeyChains, exposedCoordinates}) =>
  exposedCoordinates
  |> List.map(((idx, coordinates)) =>
       (
         idx,
         coordinates
         |> List.filter(c => snd(c))
         |> List.map(fst)
         |> List.map(c => accountKeyChains |> AccountKeyChain.find(c))
         |> List.map((a: AccountKeyChain.Address.t) => a.address),
       )
     )
  |> List.find(((_idx, addresses)) => addresses |> List.mem(address))
  |> fst;

let apply = (event: Event.t, state) =>
  switch (event) {
  | VentureCreated({ventureId, metaPolicy, network}) => {
      ...state,
      network,
      ventureId,
      payoutPolicy: metaPolicy,
    }
  | AccountCreationAccepted({data}) => {
      ...state,
      exposedCoordinates: [
        (data.accountIdx, []),
        ...state.exposedCoordinates,
      ],
      accountKeyChains: [(data.accountIdx, []), ...state.accountKeyChains],
      balance: [
        (
          data.accountIdx,
          {income: BTC.zero, spent: BTC.zero, reserved: BTC.zero},
        ),
        ...state.balance,
      ],
    }
  | AccountKeyChainUpdated(({keyChain}: AccountKeyChainUpdated.t)) => {
      ...state,
      accountKeyChains: [
        (
          keyChain.accountIdx,
          [
            (keyChain.keyChainIdx, keyChain),
            ...state.accountKeyChains |> List.assoc(keyChain.accountIdx),
          ],
        ),
        ...state.accountKeyChains |> List.remove_assoc(keyChain.accountIdx),
      ],
      nextCoordinates: [
        (
          keyChain.accountIdx,
          AccountKeyChain.Address.Coordinates.firstExternal(keyChain),
        ),
        ...state.nextCoordinates |> List.remove_assoc(keyChain.accountIdx),
      ],
      nextChangeCoordinates: [
        (
          keyChain.accountIdx,
          AccountKeyChain.Address.Coordinates.firstInternal(keyChain),
        ),
        ...state.nextCoordinates |> List.remove_assoc(keyChain.accountIdx),
      ],
    }
  | IncomeAddressExposed(({coordinates}: IncomeAddressExposed.t)) =>
    let accountIdx =
      coordinates |> AccountKeyChain.Address.Coordinates.accountIdx;
    {
      ...state,
      nextCoordinates: [
        (accountIdx, coordinates |> AccountKeyChain.Address.Coordinates.next),
        ...state.nextCoordinates |> List.remove_assoc(accountIdx),
      ],
      exposedCoordinates: [
        (
          accountIdx,
          [
            (coordinates, true),
            ...state.exposedCoordinates |> List.assoc(accountIdx),
          ],
        ),
        ...state.exposedCoordinates |> List.remove_assoc(accountIdx),
      ],
    };
  | IncomeDetected({txId, amount, address}) =>
    let accountIdx = state |> getAccountIndexOfAddress(address);
    let balance = state.balance |> List.assoc(accountIdx);
    {
      ...state,
      knownIncomeTxIds: [txId, ...state.knownIncomeTxIds],
      balance: [
        (
          accountIdx,
          {...balance, income: balance.income |> BTC.plus(amount)},
        ),
        ...state.balance |> List.remove_assoc(accountIdx),
      ],
    };
  | PayoutProposed({data, processId}) =>
    let balance = state.balance |> List.assoc(data.accountIdx);
    {
      ...state,
      reservedInputs:
        state.reservedInputs
        |> List.rev_append(data.payoutTx.usedInputs |> List.map(snd)),
      exposedCoordinates:
        switch (data.changeAddressCoordinates) {
        | None => state.exposedCoordinates
        | Some(coordinates) => [
            (
              data.accountIdx,
              [
                (coordinates, false),
                ...state.exposedCoordinates |> List.assoc(data.accountIdx),
              ],
            ),
            ...state.exposedCoordinates |> List.remove_assoc(data.accountIdx),
          ]
        },
      nextChangeCoordinates: [
        (
          data.accountIdx,
          state.nextChangeCoordinates
          |> List.assoc(data.accountIdx)
          |> AccountKeyChain.Address.Coordinates.next,
        ),
        ...state.nextCoordinates |> List.remove_assoc(data.accountIdx),
      ],
      payoutProcesses: [
        (processId, (data.accountIdx, data.payoutTx)),
        ...state.payoutProcesses,
      ],
      balance: [
        (
          data.accountIdx,
          {
            ...balance,
            reserved:
              balance.reserved
              |> BTC.plus(
                   (data.payoutTx |> PayoutTransaction.summary).reserved,
                 ),
          },
        ),
        ...state.balance |> List.remove_assoc(data.accountIdx),
      ],
    };
  | PayoutBroadcast({processId}) =>
    let (accountIdx, payoutTx) =
      state.payoutProcesses |> List.assoc(processId);
    let balance = state.balance |> List.assoc(accountIdx);
    let payoutSummary = payoutTx |> PayoutTransaction.summary;
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
      balance: [
        (
          accountIdx,
          {
            ...balance,
            spent: balance.spent |> BTC.plus(payoutSummary.spent),
            reserved: balance.reserved |> BTC.minus(payoutSummary.reserved),
          },
        ),
        ...state.balance |> List.remove_assoc(accountIdx),
      ],
    };
  | PayoutBroadcastFailed({processId}) =>
    let (accountIdx, payoutTx) =
      state.payoutProcesses |> List.assoc(processId);
    let balance = state.balance |> List.assoc(accountIdx);
    let payoutSummary = payoutTx |> PayoutTransaction.summary;
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
      balance: [
        (
          accountIdx,
          {
            ...balance,
            reserved: balance.reserved |> BTC.minus(payoutSummary.reserved),
          },
        ),
        ...state.balance |> List.remove_assoc(accountIdx),
      ],
    };
  | _ => state
  };

let getKnownTransactionIds = ({knownIncomeTxIds}) => knownIncomeTxIds;

let exposeNextIncomeAddress =
    (accountIdx, {nextCoordinates, accountKeyChains}) => {
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
        reservedInputs,
      },
    ) => {
  open AccountKeyChain.Address;
  let coordinates = exposedCoordinates |> List.assoc(accountIdx);
  let nextChangeCoordinates = nextChangeCoordinates |> List.assoc(accountIdx);
  let currentKeyChainIdx = nextChangeCoordinates |> Coordinates.keyChainIdx;
  Js.Promise.(
    accountKeyChains
    |> Network.transactionInputs(network, coordinates |> List.map(fst))
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
               ~network,
             )
           ) {
           | WithChangeAddress(payout) => (
               payout,
               Some(nextChangeCoordinates),
             )
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

let balance = (accountIdx, {balance}) => balance |> List.assoc(accountIdx);

let registerIncomeTransaction = (tx, {knownIncomeTxIds} as state) =>
  if (knownIncomeTxIds |> List.mem(tx.txId)) {
    [];
  } else {
    let exposedAddresses = state |> getExposedAddresses;
    tx.outputs
    |> List.filter(o => exposedAddresses |> List.mem(o.address))
    |> List.map(out =>
         IncomeDetected(
           IncomeDetected.make(
             ~address=out.address,
             ~txId=tx.txId,
             ~amount=out.amount,
           ),
         )
       );
  };
