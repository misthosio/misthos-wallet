open Event;

open PrimitiveTypes;

open WalletTypes;

type balance = {
  currentSpendable: BTC.t,
  reserved: BTC.t,
};

type t = {
  network: Network.t,
  accountKeyChains: AccountKeyChain.Collection.t,
  balance: list((accountIdx, balance)),
  payoutProcesses: list((ProcessId.t, (accountIdx, PayoutTransaction.t))),
};

let make = () => {
  network: Network.Testnet,
  accountKeyChains: [],
  balance: [],
  payoutProcesses: [],
};

let accountBalance = (accountIdx, {balance}) =>
  balance |> List.assoc(accountIdx);

let apply = (event: Event.t, state) =>
  switch (event) {
  | VentureCreated({network}) => {...state, network}
  | AccountCreationAccepted({data}) => {
      ...state,
      accountKeyChains: [(data.accountIdx, []), ...state.accountKeyChains],
      balance: [
        (data.accountIdx, {currentSpendable: BTC.zero, reserved: BTC.zero}),
        ...state.balance,
      ],
    }
  | AccountKeyChainIdentified(({keyChain}: AccountKeyChainIdentified.t)) => {
      ...state,
      accountKeyChains:
        state.accountKeyChains |> AccountKeyChain.Collection.add(keyChain),
    }
  | IncomeDetected({amount, coordinates}) =>
    let accountIdx = coordinates |> Address.Coordinates.accountIdx;
    let balance = state.balance |> List.assoc(accountIdx);
    {
      ...state,
      balance: [
        (
          accountIdx,
          {
            ...balance,
            currentSpendable: balance.currentSpendable |> BTC.plus(amount),
          },
        ),
        ...state.balance |> List.remove_assoc(accountIdx),
      ],
    };
  | PayoutProposed({data, processId}) =>
    let balance = state.balance |> List.assoc(data.accountIdx);
    let payoutSummary =
      data.payoutTx |> PayoutTransaction.summary(state.network);
    {
      ...state,
      payoutProcesses: [
        (processId, (data.accountIdx, data.payoutTx)),
        ...state.payoutProcesses,
      ],
      balance: [
        (
          data.accountIdx,
          {
            currentSpendable:
              balance.currentSpendable |> BTC.minus(payoutSummary.reserved),
            reserved: balance.reserved |> BTC.plus(payoutSummary.reserved),
          },
        ),
        ...state.balance |> List.remove_assoc(data.accountIdx),
      ],
    };
  | PayoutBroadcast({processId}) =>
    let (accountIdx, payoutTx) =
      state.payoutProcesses |> List.assoc(processId);
    let balance = state.balance |> List.assoc(accountIdx);
    let payoutSummary = payoutTx |> PayoutTransaction.summary(state.network);
    {
      ...state,
      balance: [
        (
          accountIdx,
          {
            currentSpendable:
              balance.currentSpendable
              |> BTC.plus(payoutSummary.reserved)
              |> BTC.minus(payoutSummary.spentWithFees),
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
    let payoutSummary = payoutTx |> PayoutTransaction.summary(state.network);
    {
      ...state,
      balance: [
        (
          accountIdx,
          {
            currentSpendable:
              balance.currentSpendable |> BTC.plus(payoutSummary.reserved),
            reserved: balance.reserved |> BTC.minus(payoutSummary.reserved),
          },
        ),
        ...state.balance |> List.remove_assoc(accountIdx),
      ],
    };
  | _ => state
  };
