open PrimitiveTypes;

open WalletTypes;

type balance = {
  income: BTC.t,
  spent: BTC.t,
  reserved: BTC.t,
};

type t = {
  accountKeyChains:
    list((accountIdx, list((accountKeyChainIdx, AccountKeyChain.t)))),
  balance: list((accountIdx, balance)),
  exposedCoordinates:
    list((accountIdx, list(AccountKeyChain.Address.Coordinates.t))),
  payoutProcesses: list((ProcessId.t, (accountIdx, PayoutTransaction.t))),
};

let make = () => {
  accountKeyChains: [],
  balance: [],
  exposedCoordinates: [],
  payoutProcesses: [],
};

let getAccountIndexOfAddress =
    (address, {accountKeyChains, exposedCoordinates}) =>
  exposedCoordinates
  |> List.map(((idx, coordinates)) =>
       (
         idx,
         coordinates
         |> List.map(c => accountKeyChains |> AccountKeyChain.find(c))
         |> List.map((a: AccountKeyChain.Address.t) => a.address),
       )
     )
  |> List.find(((_idx, addresses)) => addresses |> List.mem(address))
  |> fst;

let apply = (event: Event.t, state) =>
  switch (event) {
  | AccountCreationAccepted({data}) => {
      ...state,
      balance: [
        (
          data.accountIdx,
          {income: BTC.zero, spent: BTC.zero, reserved: BTC.zero},
        ),
        ...state.balance,
      ],
    }
  | IncomeDetected({amount, address}) =>
    let accountIdx = state |> getAccountIndexOfAddress(address);
    let balance = state.balance |> List.assoc(accountIdx);
    {
      ...state,
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
