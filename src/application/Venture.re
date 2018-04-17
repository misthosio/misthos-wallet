open PrimitiveTypes;

open WalletTypes;

let logMessage = msg => Js.log("[Venture] - " ++ msg);

module Index = Venture__Index;

module Validation = Venture__Validation;

exception InvalidEvent(Validation.result);

exception CouldNotLoadVenture;

type listener('a) = (Event.t, 'a) => 'a;

type t('a) = {
  session: Session.Data.t,
  id: ventureId,
  log: EventLog.t,
  state: Validation.state,
  wallet: Venture__Wallet.t,
  listener: listener('a),
  listenerState: 'a,
  watchers: Watchers.t,
};

module Wallet = {
  include Venture__Wallet;
  let balance = ({wallet}) => balance(AccountIndex.default, wallet);
  let getExposedAddresses = ({wallet}) =>
    getExposedAddresses(wallet) |> Array.of_list;
  let getKnownTransactionIds = ({wallet}) =>
    getKnownTransactionIds(wallet) |> Array.of_list;
};

let make = (session, id, listenerState, listener) => {
  session,
  id,
  log: EventLog.make(),
  state: Validation.makeState(),
  wallet: Wallet.make(),
  listener,
  listenerState,
  watchers: [],
};

let applyInternal =
    (issuer, event, log, (state, wallet, (listenerState, listener))) => {
  logMessage("Appending event to log:");
  logMessage(Event.encode(event) |> Json.stringify);
  let (item, log) = log |> EventLog.append(issuer, event);
  switch (item |> Validation.validate(state)) {
  | Ok =>
    let state = state |> Validation.apply(event);
    let wallet = wallet |> Wallet.apply(event);
    let listenerState = listenerState |> listener(event);
    (item, log, (state, wallet, (listenerState, listener)));
  /* This should never happen / only incase of an UI input bug!!! */
  | result =>
    logMessage("Event was rejected because of:");
    logMessage(Validation.resultToString(result));
    raise(InvalidEvent(result));
  };
};

let apply =
    (
      ~systemEvent=false,
      event,
      {session, id, log, state, wallet, listenerState, listener, watchers},
    ) => {
  let (item, log, (state, wallet, (listenerState, listener))) =
    applyInternal(
      systemEvent ? state.systemIssuer : session.issuerKeyPair,
      event,
      log,
      (state, wallet, (listenerState, listener)),
    );
  Js.Promise.(
    watchers
    |> Watchers.applyAndProcessPending(
         session,
         item,
         log,
         applyInternal,
         (state, wallet, (listenerState, listener)),
       )
    |> then_(((log, (state, wallet, (listenerState, listener)), watchers)) =>
         {session, id, log, state, wallet, listenerState, listener, watchers}
         |> resolve
       )
  );
};

let reconstruct = (session, listenerState, listener, log) => {
  let {state, wallet} =
    make(session, VentureId.make(), listenerState, listener);
  let (id, state, wallet, (listenerState, listener), watchers) =
    log
    |> EventLog.reduce(
         (
           (id, state, wallet, (listenerState, listener), watchers),
           {event} as item,
         ) => (
           switch (event) {
           | VentureCreated({ventureId}) => ventureId
           | _ => id
           },
           state |> Validation.apply(event),
           wallet |> Wallet.apply(event),
           (listenerState |> listener(event), listener),
           watchers |> Watchers.apply(~reconstruct=true, session, item, log),
         ),
         (VentureId.make(), state, wallet, (listenerState, listener), []),
       );
  watchers
  |> Watchers.processPending(
       session,
       log,
       applyInternal,
       (state, wallet, (listenerState, listener)),
     )
  |> Js.Promise.then_(
       ((log, (state, wallet, (listenerState, listener)), watchers)) =>
       {session, id, log, state, wallet, listenerState, listener, watchers}
       |> Js.Promise.resolve
     );
};

let persist = ({id, log, state} as venture) => {
  let logString = log |> EventLog.encode |> Json.stringify;
  let summaryString =
    log |> EventLog.getSummary |> EventLog.encodeSummary |> Json.stringify;
  let returnPromise =
    Js.Promise.(
      Blockstack.putFileEncrypted(
        (id |> VentureId.toString) ++ "/log.json",
        logString,
      )
      |> then_(() => resolve(venture))
    );
  Js.Promise.(
    state.partnerStoragePrefixes
    |> List.fold_left(
         (promise, (pubKey, prefix)) =>
           promise
           |> then_(() =>
                Blockstack.putFileNotEncrypted(
                  (id |> VentureId.toString) ++ "/" ++ prefix ++ "/log.json",
                  logString
                  |> Blockstack.encryptECIES(~publicKey=pubKey)
                  |> Json.stringify,
                )
              )
           |> then_(() =>
                Blockstack.putFileNotEncrypted(
                  (id |> VentureId.toString)
                  ++ "/"
                  ++ prefix
                  ++ "/summary.json",
                  summaryString,
                )
              ),
         resolve(),
       )
    |> ignore
  );
  returnPromise;
};

let defaultPolicy = Policy.absolute;

let load = (session: Session.Data.t, ~ventureId, ~listenerState, ~listener) => {
  logMessage("Loading venture '" ++ VentureId.toString(ventureId) ++ "'");
  Js.Promise.(
    Blockstack.getFileDecrypted(
      (ventureId |> VentureId.toString) ++ "/log.json",
    )
    |> then_(nullLog =>
         switch (Js.Nullable.toOption(nullLog)) {
         | Some(raw) =>
           raw
           |> Json.parseOrRaise
           |> EventLog.decode
           |> reconstruct(session, listenerState, listener)
         | None => raise(CouldNotLoadVenture)
         }
       )
    |> then_(persist)
  );
};

let join =
    (
      session: Session.Data.t,
      ~userId: string,
      ~ventureId,
      ~listenerState,
      ~listener,
    ) =>
  Js.Promise.(
    Blockstack.getFileFromUserAndDecrypt(
      ventureId ++ "/" ++ session.storagePrefix ++ "/log.json",
      ~username=userId,
    )
    |> catch(_error => raise(Not_found))
    |> then_(nullFile =>
         switch (Js.Nullable.toOption(nullFile)) {
         | None => raise(Not_found)
         | Some(raw) =>
           raw
           |> Json.parseOrRaise
           |> EventLog.decode
           |> reconstruct(session, listenerState, listener)
         }
       )
    |> then_(persist)
    |> then_(venture =>
         Index.add(
           ~ventureId=venture.id,
           ~ventureName=venture.state.ventureName,
         )
         |> then_(index => resolve((index, venture)))
       )
  );

let getId = ({id}) => id |> VentureId.toString;

let getSummary = ({log}) => log |> EventLog.getSummary;

let getListenerState = ({listenerState}) => listenerState;

module SynchronizeLogs = {
  let getPartnerHistoryUrls = ({session, id, state}) =>
    state.partnerIds
    |> List.filter(partnerId => UserId.neq(partnerId, session.userId))
    |> List.map(partnerId =>
         Blockstack.getUserAppFileUrl(
           ~path=(id |> VentureId.toString) ++ "/" ++ session.storagePrefix,
           ~username=partnerId |> UserId.toString,
           ~appOrigin=Location.origin,
         )
       )
    |> Array.of_list
    |> Js.Promise.all;
  type result('a) =
    | Ok(t('a))
    | Error(t('a), EventLog.item, Validation.result);
  let exec = (otherLogs, {session, log} as venture) => {
    let otherLogs =
      otherLogs
      |> List.map(encryptedLog =>
           encryptedLog
           |> Blockstack.decryptECIES(~privateKey=session.appPrivateKey)
           |> Json.parseOrRaise
           |> EventLog.decode
         );
    let newItems = log |> EventLog.findNewItems(otherLogs);
    let ({log, state, wallet, listenerState, listener, watchers}, error) =
      newItems
      |> List.fold_left(
           (
             (
               {log, watchers, state, wallet, listenerState, listener} as venture,
               error,
             ),
             {event} as item: EventLog.item,
           ) =>
             if (Js.Option.isSome(error)) {
               (venture, error);
             } else {
               switch (item |> Validation.validate(state)) {
               | Ok =>
                 let log = log |> EventLog.appendItem(item);
                 let state = state |> Validation.apply(event);
                 let wallet = wallet |> Wallet.apply(event);
                 let listenerState = listenerState |> listener(event);
                 let watchers =
                   watchers |> Watchers.apply(session, item, log);
                 (
                   {...venture, log, watchers, state, wallet, listenerState},
                   None,
                 );
               | PolicyMissmatch as conflict => (
                   venture,
                   Some(Error(venture, item, conflict)),
                 )
               | PolicyNotFulfilled as conflict => (
                   venture,
                   Some(Error(venture, item, conflict)),
                 )
               /* Ignored validation issues */
               | InvalidIssuer =>
                 logMessage("Invalid issuer detected");
                 (venture, None);
               | BadData(msg) =>
                 logMessage("Bad data in event detected: " ++ msg);
                 (venture, None);
               | UnknownProcessId =>
                 logMessage("Unknown ProcessId detected");
                 (venture, None);
               | DuplicateEndorsement =>
                 logMessage("Duplicate Endorsement detected");
                 (venture, None);
               | DependencyNotMet =>
                 logMessage("Dependency Not Met detected");
                 (venture, None);
               };
             },
           (venture, None),
         );
    Js.Promise.(
      watchers
      |> Watchers.processPending(
           session,
           log,
           applyInternal,
           (state, wallet, (listenerState, listener)),
         )
      |> then_(
           ((log, (state, wallet, (listenerState, listener)), watchers)) =>
           {...venture, log, state, wallet, listener, listenerState, watchers}
           |> persist
         )
      |> then_(p =>
           (
             switch (error) {
             | None => Ok(p)
             | Some(e) => e
             }
           )
           |> resolve
         )
    );
  };
};

let getPartnerHistoryUrls = SynchronizeLogs.getPartnerHistoryUrls;

module Cmd = {
  module Create = {
    type result('a) = (Index.t, t('a));
    let exec =
        (
          session: Session.Data.t,
          ~name as ventureName,
          ~listenerState,
          ~listener,
        ) => {
      logMessage("Executing 'Create' command");
      let ventureCreated =
        Event.VentureCreated.make(
          ~ventureName,
          ~creatorId=session.userId,
          ~creatorPubKey=session.issuerKeyPair |> Utils.publicKeyFromKeyPair,
          ~metaPolicy=defaultPolicy,
          ~network=session.network,
        );
      Js.(
        Promise.all2((
          Index.add(~ventureId=ventureCreated.ventureId, ~ventureName),
          make(session, ventureCreated.ventureId, listenerState, listener)
          |> apply(VentureCreated(ventureCreated))
          |> Promise.then_(persist),
        ))
      );
    };
  };
  module SynchronizeLogs = SynchronizeLogs;
  module SynchronizeWallet = {
    type result('a) =
      | Ok(t('a));
    let exec = (newTransactions: list(transaction), {wallet} as venture) => {
      let events =
        newTransactions
        |> List.map(tx => wallet |> Wallet.registerIncomeTransaction(tx))
        |> List.flatten;
      Js.Promise.(
        events
        |> List.fold_left(
             (p, event) =>
               p |> then_(v => v |> apply(~systemEvent=true, event)),
             venture |> resolve,
           )
        |> then_(persist)
        |> then_(venture => Ok(venture) |> resolve)
      );
    };
  };
  module ProposePartner = {
    type result('a) =
      | Ok(t('a))
      | NoUserInfo;
    let exec = (~prospectId, {session, state} as venture) => {
      logMessage("Executing 'ProposePartner' command");
      Js.Promise.(
        UserInfo.Public.read(~blockstackId=prospectId)
        |> then_(
             fun
             | UserInfo.Public.Ok(info) => {
                 let partnerProposal =
                   Event.makePartnerProposed(
                     ~supporterId=session.userId,
                     ~prospectId,
                     ~prospectPubKey=info.appPubKey,
                     ~policy=
                       state.policies |> List.assoc(Event.Partner.processName),
                   )
                   |> Event.getPartnerProposedExn;
                 let custodianProposal =
                   Event.makeCustodianProposed(
                     ~dependsOn=Some(partnerProposal.processId),
                     ~supporterId=session.userId,
                     ~partnerId=prospectId,
                     ~accountIdx=AccountIndex.default,
                     ~policy=
                       state.policies
                       |> List.assoc(Event.Custodian.processName),
                   )
                   |> Event.getCustodianProposedExn;
                 venture
                 |> apply(Event.PartnerProposed(partnerProposal))
                 |> then_(
                      apply(Event.CustodianProposed(custodianProposal)),
                    )
                 |> then_(persist)
                 |> then_(p => resolve(Ok(p)));
               }
             | UserInfo.Public.NotFound => resolve(NoUserInfo),
           )
      );
    };
  };
  module EndorsePartner = {
    type result('a) =
      | Ok(t('a));
    let exec = (~processId, {state, session} as venture) => {
      logMessage("Executing 'EndorsePartner' command");
      let {id: partnerId}: Event.Partner.Data.t =
        state.partnerData |> List.assoc(processId);
      let (custodianProcessId, _) =
        state.custodianData
        |> List.find(((_, data: Event.Custodian.Data.t)) =>
             data.partnerId == partnerId
           );
      Js.Promise.(
        venture
        |> apply(
             Event.makePartnerEndorsed(
               ~processId,
               ~supporterId=session.userId,
             ),
           )
        |> then_(
             apply(
               Event.makeCustodianEndorsed(
                 ~processId=custodianProcessId,
                 ~supporterId=session.userId,
               ),
             ),
           )
        |> then_(persist)
        |> then_(p => resolve(Ok(p)))
      );
    };
  };
  module ExposeIncomeAddress = {
    type result('a) =
      | Ok(string, t('a));
    let exec = (~accountIdx, {wallet} as venture) => {
      logMessage("Executing 'GetIncomeAddress' command");
      let exposeEvent = wallet |> Wallet.exposeNextIncomeAddress(accountIdx);
      Js.Promise.(
        venture
        |> apply(IncomeAddressExposed(exposeEvent))
        |> then_(persist)
        |> then_(p => resolve(Ok(exposeEvent.address, p)))
      );
    };
  };
  module ProposePayout = {
    type result('a) =
      | Ok(t('a));
    let exec =
        (~accountIdx, ~destinations, ~fee, {wallet, session} as venture) => {
      logMessage("Executing 'ProposePayout' command");
      Js.Promise.(
        Wallet.preparePayoutTx(session, accountIdx, destinations, fee, wallet)
        |> then_(proposal => venture |> apply(PayoutProposed(proposal)))
        |> then_(persist)
        |> then_(p => resolve(Ok(p)))
      );
    };
  };
  module EndorsePayout = {
    type result('a) =
      | Ok(t('a));
    let exec = (~processId, {session} as venture) => {
      logMessage("Executing 'EndorsePayout' command");
      Js.Promise.(
        venture
        |> apply(
             Event.makePayoutEndorsed(
               ~processId,
               ~supporterId=session.userId,
             ),
           )
        |> then_(persist)
        |> then_(p => resolve(Ok(p)))
      );
    };
  };
};
