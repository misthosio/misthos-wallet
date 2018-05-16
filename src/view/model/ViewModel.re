open PrimitiveTypes;

open WalletTypes;

module PartnersCollector = ViewModel__PartnersCollector;

module BalanceCollector = ViewModel__BalanceCollector;

module ManagePartners = PartnersCollector;

module Payout = {
  type t = {
    balance: BTC.t,
    ventureName: string,
  };
};

module TransactionCollector = ViewModel__TransactionCollector;

type balance = BalanceCollector.balance;

type prospect = PartnersCollector.prospect;

type partner = PartnersCollector.partner;

type confirmedTx = TransactionCollector.confirmedTx;

type unconfirmedTx = TransactionCollector.unconfirmedTx;

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

module ItemsSet = Belt.Set.String;

type t = {
  ventureId,
  name: string,
  processedItems: ItemsSet.t,
  metaPolicy: Policy.t,
  payouts: list(payout),
  balanceCollector: BalanceCollector.t,
  partnersCollector: ManagePartners.t,
  transactionCollector: TransactionCollector.t,
};

let make = () => {
  name: "",
  processedItems: ItemsSet.empty,
  ventureId: VentureId.fromString(""),
  metaPolicy: Policy.unanimous,
  payouts: [],
  balanceCollector: BalanceCollector.make(),
  partnersCollector: ManagePartners.make(),
  transactionCollector: TransactionCollector.make(),
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

let init = Array.fold_left((m, item) => m |> apply(item), make());

let applyAll = (events, model) =>
  events |> Array.fold_left((m, item) => m |> apply(item), model);

let ventureId = state => state.ventureId;

let partners = state => state.partnersCollector.partners;

let prospects = state => state.partnersCollector.prospects;

let removalProspects = state => state.partnersCollector.removalProspects;

let ventureName = state => state.name;

let payouts = state => state.payouts;

let balance = state =>
  state.balanceCollector
  |> BalanceCollector.accountBalance(AccountIndex.default);

let transactions = ({transactionCollector}) => (
  transactionCollector.confirmedTxs,
  transactionCollector.unconfirmedTxs,
);

let isPartner = (id, {partnersCollector}) =>
  partnersCollector |> PartnersCollector.isPartner(id);

let managePartnersModal = ({partnersCollector}) => partnersCollector;

let payoutModal = ({name, balanceCollector}) : Payout.t => {
  balance:
    (
      balanceCollector |> BalanceCollector.accountBalance(AccountIndex.default)
    ).
      currentSpendable,
  ventureName: name,
};
