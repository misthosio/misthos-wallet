open PrimitiveTypes;

open WalletTypes;

module ItemsSet = Belt.Set.String;

module PartnersCollector = ViewModel__PartnersCollector;

module BalanceCollector = ViewModel__BalanceCollector;

module TransactionCollector = ViewModel__TransactionCollector;

type payoutStatus =
  | PayoutPending
  | PayoutCompleted(string)
  | PayoutFailed(string);

type payout = {
  processId,
  payoutTx: PayoutTransaction.t,
  endorsedBy: list(userId),
  rejectedBy: list(userId),
  status: payoutStatus,
};

type t = {
  localUser: userId,
  ventureId,
  name: string,
  processedItems: ItemsSet.t,
  metaPolicy: Policy.t,
  payouts: list(payout),
  balanceCollector: BalanceCollector.t,
  partnersCollector: PartnersCollector.t,
  transactionCollector: TransactionCollector.t,
  walletInfoCollector: WalletInfoCollector.t,
};

let readOnly = ({localUser, partnersCollector}) =>
  partnersCollector |> PartnersCollector.isPartner(localUser) == false;

module ManagePartnersView = {
  type partner = PartnersCollector.partner;
  type t = {
    partners: list(partner),
    joinVentureUrl: string,
  };
  let fromViewModelState = ({ventureId, localUser, partnersCollector}) => {
    partners: partnersCollector.partners,
    joinVentureUrl:
      Location.origin
      ++ Router.Config.routeToUrl(JoinVenture(ventureId, localUser)),
  };
};

let managePartnersModal = ManagePartnersView.fromViewModelState;

module PayoutView = {
  type t = {
    balance: BTC.t,
    ventureName: string,
    initialSummary: PayoutTransaction.summary,
    isAddressValid: string => bool,
    max: (string, list((string, BTC.t)), BTC.t) => BTC.t,
    summary: (list((string, BTC.t)), BTC.t) => PayoutTransaction.summary,
  };
  let fromViewModelState =
      ({localUser, name, balanceCollector, walletInfoCollector}) => {
    balance:
      (
        balanceCollector
        |> BalanceCollector.accountBalance(AccountIndex.default)
      ).
        currentSpendable,
    ventureName: name,
    initialSummary: {
      reserved: BTC.zero,
      spentWithFees: BTC.zero,
      misthosFee: BTC.zero,
      networkFee: BTC.zero,
    },
    isAddressValid: address =>
      try (
        {
          Bitcoin.Address.toOutputScript(
            address,
            walletInfoCollector.network |> Network.bitcoinNetwork,
          )
          |> ignore;
          true;
        }
      ) {
      | _ => false
      },
    max: (targetDestination, destinations, fee) =>
      PayoutTransaction.max(
        ~allInputs=walletInfoCollector.unused,
        ~targetDestination,
        ~destinations,
        ~satsPerByte=fee,
        ~network=walletInfoCollector.network,
      ),
    summary: (destinations, fee) =>
      PayoutTransaction.build(
        ~mandatoryInputs=
          walletInfoCollector
          |> WalletInfoCollector.oldInputs(AccountIndex.default, localUser),
        ~allInputs=walletInfoCollector.unused,
        ~destinations,
        ~satsPerByte=fee,
        ~changeAddress=
          walletInfoCollector
          |> WalletInfoCollector.nextChangeAddress(
               AccountIndex.default,
               localUser,
             ),
        ~network=walletInfoCollector.network,
      )
      |> PayoutTransaction.summary(walletInfoCollector.network),
  };
};

let payoutModal = PayoutView.fromViewModelState;

module SelectedVentureView = {
  type partner = PartnersCollector.partner;
  type prospect = PartnersCollector.prospect;
  type confirmedTx = TransactionCollector.confirmedTx;
  type unconfirmedTx = TransactionCollector.unconfirmedTx;
  type nonrec payoutStatus = payoutStatus;
  type nonrec payout = payout;
  type balance = BalanceCollector.balance;
  type t = {
    ventureId,
    ventureName: string,
    readOnly: bool,
    partners: list(partner),
    prospects: list(prospect),
    removalProspects: list(prospect),
    transactions: (list(confirmedTx), list(unconfirmedTx)),
    payouts: list(payout),
    balance,
  };
  let fromViewModelState =
      (
        {
          ventureId,
          name,
          localUser,
          partnersCollector,
          transactionCollector,
          payouts,
          balanceCollector,
        },
      ) => {
    ventureId,
    ventureName: name,
    readOnly:
      partnersCollector |> PartnersCollector.isPartner(localUser) == false,
    partners: partnersCollector.partners,
    prospects: partnersCollector.prospects,
    removalProspects: partnersCollector.removalProspects,
    transactions: (
      transactionCollector.confirmedTxs,
      transactionCollector.unconfirmedTxs,
    ),
    payouts,
    balance:
      balanceCollector
      |> BalanceCollector.accountBalance(AccountIndex.default),
  };
};

let selectedVenture = SelectedVentureView.fromViewModelState;

let make = localUser => {
  localUser,
  name: "",
  processedItems: ItemsSet.empty,
  ventureId: VentureId.fromString(""),
  metaPolicy: Policy.unanimous,
  payouts: [],
  balanceCollector: BalanceCollector.make(),
  partnersCollector: PartnersCollector.make(localUser),
  transactionCollector: TransactionCollector.make(),
  walletInfoCollector: WalletInfoCollector.make(),
};

let apply = ({event, hash}: EventLog.item, {processedItems} as state) =>
  if (processedItems |. ItemsSet.has(hash)) {
    state;
  } else {
    let state = {
      ...state,
      balanceCollector:
        state.balanceCollector |> BalanceCollector.apply(event),
      partnersCollector:
        state.partnersCollector |> PartnersCollector.apply(event),
      transactionCollector:
        state.transactionCollector |> TransactionCollector.apply(event),
      walletInfoCollector:
        state.walletInfoCollector |> WalletInfoCollector.apply(event),
      processedItems: processedItems |. ItemsSet.add(hash),
    };
    switch (event) {
    | VentureCreated({ventureName, metaPolicy, ventureId}) => {
        ...state,
        ventureId,
        name: ventureName,
        metaPolicy,
      }
    | PayoutProposed({processId, supporterId, data}) => {
        ...state,
        payouts: [
          {
            processId,
            payoutTx: data.payoutTx,
            endorsedBy: [supporterId],
            rejectedBy: [],
            status: PayoutPending,
          },
          ...state.payouts,
        ],
      }
    | PayoutRejected({processId, rejectorId}) => {
        ...state,
        payouts:
          state.payouts
          |> List.map((p: payout) =>
               ProcessId.eq(p.processId, processId) ?
                 {...p, rejectedBy: [rejectorId, ...p.rejectedBy]} : p
             ),
      }
    | PayoutEndorsed({processId, supporterId}) => {
        ...state,
        payouts:
          state.payouts
          |> List.map((p: payout) =>
               ProcessId.eq(p.processId, processId) ?
                 {...p, endorsedBy: [supporterId, ...p.endorsedBy]} : p
             ),
      }
    | PayoutBroadcast({processId, txId}) => {
        ...state,
        payouts:
          state.payouts
          |> List.map((p: payout) =>
               ProcessId.eq(p.processId, processId) ?
                 {...p, status: PayoutCompleted(txId)} : p
             ),
      }
    | PayoutBroadcastFailed({processId, errorMessage}) => {
        ...state,
        payouts:
          state.payouts
          |> List.map((p: payout) =>
               ProcessId.eq(p.processId, processId) ?
                 {...p, status: PayoutFailed(errorMessage)} : p
             ),
      }
    | _ => state
    };
  };

let init = localUser =>
  EventLog.reduce((m, item) => m |> apply(item), make(localUser));

let applyAll = (events, model) =>
  events |> Array.fold_left((m, item) => m |> apply(item), model);
