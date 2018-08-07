open PrimitiveTypes;

open WalletTypes;

module ItemsSet = Belt.Set.String;

module PartnersCollector = ViewModel__PartnersCollector;

module TransactionCollector = ViewModel__TransactionCollector;

module TxDetailsCollector = ViewModel__TxDetailsCollector;

module OldInputCollector = ViewModel__OldTxInputCollector;

type t = {
  localUser: userId,
  ventureId,
  lastResponse:
    option((WebWorker.correlationId, VentureWorkerMessage.cmdResponse)),
  ventureName: string,
  processedItems: ItemsSet.t,
  partnersCollector: PartnersCollector.t,
  transactionCollector: TransactionCollector.t,
  txDetailsCollector: TxDetailsCollector.t,
  oldInputCollector: OldInputCollector.t,
  walletInfoCollector: WalletInfoCollector.t,
  ledgerInfoCollector: LedgerInfoCollector.t,
};

let readOnly = ({localUser, partnersCollector}) =>
  partnersCollector |> PartnersCollector.isPartner(localUser) == false;

let captureResponse = (correlationId, response, state) => {
  ...state,
  lastResponse: Some((correlationId, response)),
};

let lastResponse = ({lastResponse}) => lastResponse;

module VentureSettingsView = {
  type t = {
    ledgerId: option(string),
    ledgerUpToDate: bool,
    getCustodianKeyChain: unit => Js.Promise.t(Ledger.result),
  };
  let fromViewModel = ({ventureId, walletInfoCollector, ledgerInfoCollector}) => {
    ledgerId:
      ledgerInfoCollector
      |> LedgerInfoCollector.ledgerId(AccountIndex.default),
    ledgerUpToDate:
      ledgerInfoCollector
      |> LedgerInfoCollector.ledgerUpToDate(AccountIndex.default),
    getCustodianKeyChain: () =>
      Ledger.getCustodianKeyChain(
        ~network=walletInfoCollector |> WalletInfoCollector.network,
        ~ventureId,
        ~ledgerId=
          ledgerInfoCollector
          |> LedgerInfoCollector.ledgerId(AccountIndex.default),
        ~accountIdx=AccountIndex.default,
        ~keyChainIdx=
          ledgerInfoCollector
          |> LedgerInfoCollector.nextKeyChainIdx(AccountIndex.default),
      ),
  };
};
let ventureSettingsView = VentureSettingsView.fromViewModel;

module AddressesView = {
  open Belt;
  type addressType = WalletInfoCollector.addressType;
  type addressStatus = WalletInfoCollector.addressStatus;
  type addressInfo = WalletInfoCollector.addressInfo;
  type incomeStatus = TxDetailsCollector.incomeStatus;
  type income = {
    status: incomeStatus,
    unlocked: bool,
    date: option(Js.Date.t),
    txId: string,
    amount: BTC.t,
    detailsLink: Router.Config.route,
  };
  type addressDetails = {
    custodians: UserId.set,
    usingHardwareKey: UserId.set,
    nCoSigners: int,
    nCustodians: int,
    addressType,
    addressStatus,
    unspentIncome: list(income),
    spentIncome: list(income),
    isPartner: UserId.t => bool,
  };
  type t = {
    infos: list(addressInfo),
    ventureId,
    atRiskWarning: bool,
    addressDetails: addressInfo => addressDetails,
  };
  let fromViewModelState =
      (
        {
          walletInfoCollector,
          oldInputCollector,
          partnersCollector,
          txDetailsCollector,
          ventureId,
        },
      ) => {
    let infos =
      walletInfoCollector
      |> WalletInfoCollector.addressInfos(AccountIndex.default);

    {
      infos,
      ventureId,
      atRiskWarning:
        infos
        |. List.reduceU(
             false, (. res, {addressStatus, balance}: addressInfo) =>
             switch (addressStatus) {
             | AtRisk => res || balance |. BTC.gt(BTC.zero)
             | _ => res
             }
           ),
      addressDetails: addressInfo => {
        isPartner: id => partnersCollector |> PartnersCollector.isPartner(id),
        custodians: addressInfo.custodians,
        usingHardwareKey: addressInfo.usingHardwareKey,
        nCustodians: addressInfo.custodians |> Belt.Set.size,
        nCoSigners: addressInfo.nCoSigners,
        addressType: addressInfo.addressType,
        addressStatus: addressInfo.addressStatus,
        unspentIncome:
          WalletInfoCollector.inputsFor(
            AccountIndex.default,
            addressInfo,
            walletInfoCollector,
          )
          |. Belt.List.mapU((. {txId, value, unlocked}: Network.txInput) => {
               let (date, status) =
                 txDetailsCollector
                 |> TxDetailsCollector.getDateAndStatus(txId);
               let detailsLink: Router.Config.route =
                 switch (addressInfo.addressType) {
                 | Income(_) => Venture(ventureId, Income(txId))
                 | Change =>
                   Venture(
                     ventureId,
                     Payout(
                       txDetailsCollector
                       |> TxDetailsCollector.getProcessIdForTx(txId),
                     ),
                   )
                 };
               {txId, amount: value, status, date, unlocked, detailsLink};
             }),
        spentIncome:
          oldInputCollector
          |> OldInputCollector.inputsFor(addressInfo.address)
          |. Belt.List.mapU((. {txId, value, unlocked}: Network.txInput) => {
               let (date, status) =
                 txDetailsCollector
                 |> TxDetailsCollector.getDateAndStatus(txId);
               let detailsLink: Router.Config.route =
                 switch (addressInfo.addressType) {
                 | Income(_) => Venture(ventureId, Income(txId))
                 | Change =>
                   Venture(
                     ventureId,
                     Payout(
                       txDetailsCollector
                       |> TxDetailsCollector.getProcessIdForTx(txId),
                     ),
                   )
                 };
               {txId, amount: value, status, date, unlocked, detailsLink};
             }),
      },
    };
  };
};
let viewAddressesModal = AddressesView.fromViewModelState;

module ManagePartnersView = {
  open Belt;
  type partner = PartnersCollector.partner;
  type t = {
    ventureName: string,
    partners: list(partner),
    alertPartners: UserId.set,
  };
  let fromViewModelState =
      ({ventureName, partnersCollector, walletInfoCollector}) => {
    let infos =
      walletInfoCollector
      |> WalletInfoCollector.addressInfos(AccountIndex.default);

    {
      alertPartners:
        infos
        |. Belt.List.reduceU(
             UserId.emptySet,
             (.
               res,
               {addressStatus, custodians, balance}: WalletInfoCollector.addressInfo,
             ) =>
             switch (addressStatus, balance) {
             | (AtRisk, balance)
             | (TemporarilyInaccessible, balance)
                 when balance |. BTC.gt(BTC.zero) =>
               res |. Set.union(custodians)
             | _ => res
             }
           ),
      ventureName,
      partners: partnersCollector.partners,
    };
  };
};

let managePartnersModal = ManagePartnersView.fromViewModelState;

module ViewPartnerView = {
  open Belt;
  type voteStatus = ProcessCollector.voteStatus;
  type voter = ProcessCollector.voter;
  type partnerProcess = PartnersCollector.partnerProcess;
  type t = {
    localUser: userId,
    ventureName: string,
    partnerProcess,
    currentPartners: UserId.set,
    atRiskWarning: bool,
    joinVentureUrl: string,
    webDomain: string,
  };
  let environment = Environment.get();
  let fromViewModelState =
      (
        processId,
        {
          ventureName,
          ventureId,
          localUser,
          partnersCollector,
          walletInfoCollector,
        },
      ) =>
    partnersCollector
    |> PartnersCollector.getPartnerProcess(processId)
    |> Utils.mapOption(partnerProcess =>
         {
           currentPartners:
             partnersCollector |> PartnersCollector.currentPartners,
           localUser,
           ventureName,
           partnerProcess,
           webDomain: environment.webDomain,
           joinVentureUrl:
             environment.appDomain
             ++ Router.Config.routeToUrl(JoinVenture(ventureId, localUser)),
           atRiskWarning:
             switch (partnerProcess.data.processType) {
             | Removal =>
               walletInfoCollector
               |> WalletInfoCollector.addressInfos(AccountIndex.default)
               |. Belt.List.reduceU(
                    false,
                    (.
                      res,
                      {addressStatus, custodians}: WalletInfoCollector.addressInfo,
                    ) =>
                    switch (addressStatus) {
                    | TemporarilyInaccessible
                    | AtRisk =>
                      res || custodians |. Set.has(partnerProcess.data.userId)
                    | _ => res
                    }
                  )
             | Addition => false
             },
         }
       );
};

let viewPartnerModal = ViewPartnerView.fromViewModelState;

module CreatePayoutView = {
  open Belt;
  type balance = {
    currentSpendable: BTC.t,
    reserved: BTC.t,
  };
  type t = {
    allowCreation: bool,
    balance,
    ventureId,
    ventureName: string,
    initialSummary: PayoutTransaction.summary,
    isAddressValid: string => bool,
    max: (string, list((string, BTC.t)), BTC.t) => BTC.t,
    summary: PayoutTransaction.t => PayoutTransaction.summary,
    createPayoutTx: (list((string, BTC.t)), BTC.t) => PayoutTransaction.t,
    requiresLedgerSig: bool,
    collectInputHexs:
      (Map.String.t(string), PayoutTransaction.t) =>
      Js.Promise.t((Map.String.t(string), array(string))),
    signPayoutTx:
      (PayoutTransaction.t, array(string)) => Js.Promise.t(Ledger.signResult),
  };
  let fromViewModelState =
      (
        {
          ventureId,
          localUser,
          ledgerInfoCollector,
          ventureName,
          walletInfoCollector,
        },
      ) => {
    let reserved =
      walletInfoCollector
      |> WalletInfoCollector.totalReservedBTC(AccountIndex.default);
    let balance = {
      reserved,
      currentSpendable:
        walletInfoCollector
        |> WalletInfoCollector.totalUnusedBTC(AccountIndex.default)
        |. BTC.minus(reserved),
    };
    let network = walletInfoCollector |> WalletInfoCollector.network;
    let optionalInputs =
      walletInfoCollector
      |> WalletInfoCollector.currentSpendableInputs(AccountIndex.default);
    let mandatoryInputs =
      walletInfoCollector
      |> WalletInfoCollector.oldSpendableInputs(AccountIndex.default);
    let unlockedInputs =
      walletInfoCollector
      |> WalletInfoCollector.unlockedInputs(AccountIndex.default);
    let allInputs =
      optionalInputs
      |. Belt.Set.union(mandatoryInputs)
      |. Belt.Set.union(unlockedInputs);
    let changeAddress =
      walletInfoCollector
      |> WalletInfoCollector.nextChangeAddress(
           AccountIndex.default,
           localUser,
         );
    {
      ventureId,
      balance,
      allowCreation: balance.currentSpendable |. BTC.gt(BTC.zero),
      ventureName,
      initialSummary: {
        reserved: BTC.zero,
        destinations: [],
        spentWithFees: BTC.zero,
        misthosFee: BTC.zero,
        networkFee: BTC.zero,
      },
      isAddressValid: address =>
        try (
          {
            Bitcoin.Address.toOutputScript(
              address,
              network |> Network.bitcoinNetwork,
            )
            |> ignore;
            true;
          }
        ) {
        | _ => false
        },
      max: (targetDestination, destinations, fee) =>
        PayoutTransaction.max(
          ~allInputs,
          ~targetDestination,
          ~destinations,
          ~satsPerByte=fee,
          ~network,
        ),
      summary: PayoutTransaction.summary(network),
      createPayoutTx: (destinations, fee) =>
        PayoutTransaction.build(
          ~mandatoryInputs,
          ~unlockedInputs,
          ~optionalInputs,
          ~destinations,
          ~satsPerByte=fee,
          ~changeAddress,
          ~network,
        ),
      requiresLedgerSig:
        ledgerInfoCollector
        |> LedgerInfoCollector.ledgerId(AccountIndex.default)
        |> Js.Option.isSome,
      collectInputHexs: (knownHexs, {usedInputs}) => {
        let inputs =
          usedInputs
          |. Array.mapU((. {txId}: PayoutTransaction.input) => txId)
          |> Set.String.fromArray;
        let knownIds =
          knownHexs |. Map.String.keysToArray |> Set.String.fromArray;
        Js.Promise.(
          Set.String.diff(inputs, knownIds)
          |> Set.String.toArray
          |> NetworkClient.transactionHex(network)
          |> then_(txs => {
               let knownHexs = knownHexs |. Map.String.mergeMany(txs);
               (
                 knownHexs,
                 usedInputs
                 |. Array.mapU((. {txId}: PayoutTransaction.input) =>
                      knownHexs
                      |. Map.String.get(txId)
                      |> Js.Option.getWithDefault("")
                    ),
               )
               |> resolve;
             })
        );
      },
      signPayoutTx: (payoutTx, txHexs) =>
        Ledger.signPayout(
          ventureId,
          localUser,
          ledgerInfoCollector
          |> LedgerInfoCollector.ledgerId(AccountIndex.default)
          |> Js.Option.getWithDefault(""),
          payoutTx,
          txHexs,
          walletInfoCollector |> WalletInfoCollector.accountKeyChains,
        ),
    };
  };
};

let createPayoutModal = CreatePayoutView.fromViewModelState;

module ViewPayoutView = {
  type payoutStatus = TxDetailsCollector.payoutStatus;
  type voteStatus = ProcessCollector.voteStatus;
  type voter = ProcessCollector.voter;
  type payout = TxDetailsCollector.payoutProcess;
  type t = {
    requiresLedgerSig: bool,
    currentPartners: UserId.set,
    payout,
    collidesWith: ProcessId.set,
    signPayout: unit => Js.Promise.t(Ledger.signResult),
  };
  let fromViewModelState =
      (
        processId,
        {
          ventureId,
          localUser,
          txDetailsCollector,
          ledgerInfoCollector,
          walletInfoCollector,
          partnersCollector,
        },
      ) =>
    txDetailsCollector
    |> TxDetailsCollector.getPayout(processId)
    |> Utils.mapOption(
         ({data: {payoutTx: {usedInputs} as payoutTx}} as payout: payout) => {
         open Belt;
         let txHexPromise =
           usedInputs
           |. Array.mapU((. {txId}: PayoutTransaction.input) => txId)
           |> NetworkClient.transactionHex(
                walletInfoCollector |> WalletInfoCollector.network,
              );
         {
           signPayout: () =>
             Js.Promise.(
               txHexPromise
               |> then_(txHexs =>
                    Ledger.signPayout(
                      ventureId,
                      localUser,
                      ledgerInfoCollector
                      |> LedgerInfoCollector.ledgerId(AccountIndex.default)
                      |> Js.Option.getWithDefault(""),
                      payoutTx,
                      txHexs |. Array.map(snd),
                      walletInfoCollector
                      |> WalletInfoCollector.accountKeyChains,
                    )
                  )
             ),
           requiresLedgerSig:
             ledgerInfoCollector
             |> LedgerInfoCollector.ledgerId(AccountIndex.default)
             |> Js.Option.isSome,
           currentPartners:
             partnersCollector |> PartnersCollector.currentPartners,
           payout,
           collidesWith:
             walletInfoCollector
             |> WalletInfoCollector.collidingProcesses(
                  AccountIndex.default,
                  processId,
                ),
         };
       });
};

let viewPayoutModal = ViewPayoutView.fromViewModelState;
module ViewIncomeView = {
  type t = TxDetailsCollector.income;
  let fromViewModelState = (txId, {txDetailsCollector}) =>
    txDetailsCollector |> TxDetailsCollector.getIncome(txId);
};

let viewIncomeModal = ViewIncomeView.fromViewModelState;

module SelectedVentureView = {
  type partner = PartnersCollector.partner;
  type partnerProcess = PartnersCollector.partnerProcess;
  type txType = TransactionCollector.txType;
  type txStatus = TransactionCollector.txStatus;
  type txData = TransactionCollector.txData;
  type payoutStatus = TxDetailsCollector.payoutStatus;
  type payoutProcess = TxDetailsCollector.payoutProcess;
  type balance = {
    currentSpendable: BTC.t,
    reserved: BTC.t,
  };
  type t = {
    ventureId,
    atRiskWarning: bool,
    keyRotationWarning: bool,
    ventureName: string,
    readOnly: bool,
    partners: list(partner),
    proposedAdditions: list(partnerProcess),
    proposedRemovals: list(partnerProcess),
    unconfirmedTxs: list(txData),
    confirmedTxs: list(txData),
    payoutsPendingBroadcast: list(payoutProcess),
    balance,
  };
  let fromViewModelState =
      (
        {
          ventureId,
          ventureName,
          localUser,
          partnersCollector,
          transactionCollector,
          txDetailsCollector,
          walletInfoCollector,
          ledgerInfoCollector,
        },
      ) => {
    let reserved =
      walletInfoCollector
      |> WalletInfoCollector.totalReservedBTC(AccountIndex.default);
    let balance = {
      reserved,
      currentSpendable:
        walletInfoCollector
        |> WalletInfoCollector.totalUnusedBTC(AccountIndex.default)
        |. BTC.minus(reserved),
    };
    let (proposedAdditions, proposedRemovals) =
      partnersCollector |> PartnersCollector.processesPendingApproval;
    {
      ventureId,
      ventureName,
      keyRotationWarning:
        ledgerInfoCollector
        |> LedgerInfoCollector.ledgerId(AccountIndex.default)
        |> Js.Option.isSome
        && ! (
             ledgerInfoCollector
             |> LedgerInfoCollector.ledgerUpToDate(AccountIndex.default)
           ),
      readOnly:
        partnersCollector |> PartnersCollector.isPartner(localUser) == false,
      atRiskWarning:
        walletInfoCollector
        |> WalletInfoCollector.addressInfos(AccountIndex.default)
        |. Belt.List.reduceU(
             false,
             (.
               res,
               {addressStatus, balance}: WalletInfoCollector.addressInfo,
             ) =>
             switch (addressStatus) {
             | AtRisk => res || balance |. BTC.gt(BTC.zero)
             | _ => res
             }
           ),
      partners: partnersCollector.partners,
      proposedAdditions,
      proposedRemovals,
      payoutsPendingBroadcast:
        txDetailsCollector |> TxDetailsCollector.payoutsPendingBroadcast,
      confirmedTxs: transactionCollector.confirmedTxs,
      unconfirmedTxs: transactionCollector.unconfirmedTxs,
      balance,
    };
  };
};

let selectedVenture = SelectedVentureView.fromViewModelState;

let make = localUser => {
  localUser,
  lastResponse: None,
  ventureName: "",
  processedItems: ItemsSet.empty,
  ventureId: VentureId.fromString(""),
  partnersCollector: PartnersCollector.make(localUser),
  transactionCollector: TransactionCollector.make(),
  txDetailsCollector: TxDetailsCollector.make(localUser),
  oldInputCollector: OldInputCollector.make(),
  walletInfoCollector: WalletInfoCollector.make(),
  ledgerInfoCollector: LedgerInfoCollector.make(localUser),
};

let apply = ({event, hash}: EventLog.item, {processedItems} as state) =>
  if (processedItems |. ItemsSet.has(hash)) {
    state;
  } else {
    let state = {
      ...state,
      partnersCollector:
        state.partnersCollector |> PartnersCollector.apply(event),
      transactionCollector:
        state.transactionCollector |> TransactionCollector.apply(event),
      txDetailsCollector:
        state.txDetailsCollector |> TxDetailsCollector.apply(event),
      walletInfoCollector:
        state.walletInfoCollector |> WalletInfoCollector.apply(event),
      oldInputCollector:
        state.oldInputCollector |> OldInputCollector.apply(event),
      ledgerInfoCollector:
        state.ledgerInfoCollector |> LedgerInfoCollector.apply(event),
      processedItems: processedItems |. ItemsSet.add(hash),
    };
    switch (event) {
    | VentureCreated({ventureName, ventureId}) => {
        ...state,
        ventureId,
        ventureName,
      }
    | _ => state
    };
  };

let init = localUser =>
  EventLog.reduce((m, item) => m |> apply(item), make(localUser));

let applyAll = (events, model) =>
  events |> Array.fold_left((m, item) => m |> apply(item), model);
