open PrimitiveTypes;

open WalletTypes;

let logMessage = msg => Js.log("[Venture] - " ++ msg);

module Index = Venture__Index;

module Validation = Venture__Validation;

module State = Venture__State;

exception InvalidEvent(Validation.result);

exception CouldNotLoadVenture;

type t = {
  session: Session.Data.t,
  id: ventureId,
  log: EventLog.t,
  state: State.t,
  validation: Validation.t,
  wallet: Venture__Wallet.t,
  watchers: Watchers.t,
};

module Wallet = Venture__Wallet;

let make = (session, id) => {
  session,
  id,
  log: EventLog.make(),
  state: State.make(),
  validation: Validation.make(),
  wallet: Wallet.make(),
  watchers: [],
};

let applyInternal =
    (
      ~syncing=false,
      issuer,
      event,
      oldLog,
      (validation, state, wallet, collector),
    ) => {
  let (item, log) = oldLog |> EventLog.append(issuer, event);
  switch (item |> Validation.validate(validation)) {
  | Ok =>
    logMessage("Appended event to log:");
    logMessage(Event.encode(event) |> Json.stringify);
    let validation = validation |> Validation.apply(item);
    let state = state |> State.apply(event);
    let wallet = wallet |> Wallet.apply(event);
    let collector = [event, ...collector];
    (Some(item), log, (validation, state, wallet, collector));
  | Ignore =>
    logMessage("Ignoring event:");
    logMessage(Event.encode(event) |> Json.stringify);
    (None, oldLog, (validation, state, wallet, collector));
  | result =>
    logMessage("Event was rejected because of:");
    logMessage(Validation.resultToString(result));
    syncing ?
      (None, oldLog, (validation, state, wallet, collector)) :
      /* This should never happen / only incase of an UI input bug!!! */
      raise(InvalidEvent(result));
  };
};

let apply =
    (
      ~systemEvent=false,
      ~collector=[],
      event,
      {session, id, log, validation, state, wallet, watchers},
    ) => {
  let (item, log, (validation, state, wallet, collector)) =
    applyInternal(
      systemEvent ? state |> State.systemIssuer : session.issuerKeyPair,
      event,
      log,
      (validation, state, wallet, collector),
    );
  Js.Promise.(
    watchers
    |> Watchers.applyAndProcessPending(
         session,
         item,
         log,
         applyInternal,
         (validation, state, wallet, collector),
       )
    |> then_(((log, (validation, state, wallet, collector), watchers)) =>
         ({validation, session, id, log, state, wallet, watchers}, collector)
         |> resolve
       )
  );
};

let reconstruct = (session, log) => {
  let {validation, state, wallet} = make(session, VentureId.make());
  let (id, validation, state, wallet, collector, watchers) =
    log
    |> EventLog.reduce(
         (
           (id, validation, state, wallet, collector, watchers),
           {event} as item,
         ) => (
           switch (event) {
           | VentureCreated({ventureId}) => ventureId
           | _ => id
           },
           validation |> Validation.apply(item),
           state |> State.apply(event),
           wallet |> Wallet.apply(event),
           [event, ...collector],
           watchers
           |> Watchers.apply(~reconstruct=true, session, Some(item), log),
         ),
         (VentureId.make(), validation, state, wallet, [], []),
       );
  watchers
  |> Watchers.processPending(
       session,
       log,
       applyInternal,
       (validation, state, wallet, collector),
     )
  |> Js.Promise.then_(
       ((log, (validation, state, wallet, collector), watchers)) =>
       ({validation, session, id, log, state, wallet, watchers}, collector)
       |> Js.Promise.resolve
     );
};

let persist = (({id, log} as venture, collector)) =>
  Js.Promise.(
    Blockstack.putFileEncrypted(
      (id |> VentureId.toString) ++ "/log.json",
      log |> EventLog.encode |> Json.stringify,
    )
    |> then_(() => resolve((venture, collector)))
  );

let defaultPolicy = Policy.unanimous;

let load = (session: Session.Data.t, ~ventureId) => {
  logMessage("Loading venture '" ++ VentureId.toString(ventureId) ++ "'");
  Js.Promise.(
    Blockstack.getFileDecrypted(
      (ventureId |> VentureId.toString) ++ "/log.json",
    )
    |> then_(nullLog =>
         switch (Js.Nullable.toOption(nullLog)) {
         | Some(raw) =>
           raw |> Json.parseOrRaise |> EventLog.decode |> reconstruct(session)
         | None => raise(CouldNotLoadVenture)
         }
       )
    |> then_(persist)
    |> then_(((v, _)) => v |> resolve)
  );
};

let join = (session: Session.Data.t, ~userId, ~ventureId) =>
  Js.Promise.(
    load(session, ~ventureId)
    |> then_(venture => all2((Index.load(), venture |> resolve)))
    |> catch(_error =>
         Blockstack.getFileFromUserAndDecrypt(
           (ventureId |> VentureId.toString)
           ++ "/"
           ++ session.storagePrefix
           ++ "/log.json",
           ~username=userId |> UserId.toString,
         )
         |> catch(_error => raise(Not_found))
         |> then_(nullFile =>
              switch (Js.Nullable.toOption(nullFile)) {
              | None => raise(Not_found)
              | Some(raw) =>
                raw
                |> Json.parseOrRaise
                |> EventLog.decode
                |> reconstruct(session)
              }
            )
         |> then_(persist)
         |> then_(((venture, _)) =>
              Index.add(
                ~ventureId=venture.id,
                ~ventureName=venture.state |> State.ventureName,
              )
              |> then_(index => resolve((index, venture)))
            )
       )
  );

let getId = ({id}) => id;

let getSummary = ({log}) => log |> EventLog.getSummary;

let getAllEvents = ({log}) =>
  log |> EventLog.reduce((l, {event}) => [event, ...l], []);

module Cmd = {
  module Create = {
    type result = (Index.t, t);
    let exec = (session: Session.Data.t, ~name as ventureName) => {
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
        ventureCreated.ventureId,
        Promise.all2((
          Index.add(~ventureId=ventureCreated.ventureId, ~ventureName),
          make(session, ventureCreated.ventureId)
          |> apply(VentureCreated(ventureCreated))
          |> Promise.then_(persist)
          |> Promise.then_(((v, _)) => v |> Promise.resolve),
        )),
      );
    };
  };
  module SynchronizeLogs = {
    type result =
      | Ok(t, list(Event.t))
      | Error(t, EventLog.item, Validation.result);
    let exec = (newItems, {session} as venture) => {
      let ({log, validation, state, wallet, watchers}, collector, error) =
        newItems
        |> List.fold_left(
             (
               (
                 {log, watchers, validation, state, wallet} as venture,
                 collector,
                 error,
               ),
               {event} as item: EventLog.item,
             ) =>
               if (Js.Option.isSome(error)) {
                 (venture, collector, error);
               } else {
                 switch (item |> Validation.validate(validation)) {
                 | Ok =>
                   logMessage("Appending synced event to log:");
                   logMessage(Event.encode(event) |> Json.stringify);
                   let log = log |> EventLog.appendItem(item);
                   let validation = validation |> Validation.apply(item);
                   let state = state |> State.apply(event);
                   let wallet = wallet |> Wallet.apply(event);
                   let collector = [event, ...collector];
                   let watchers =
                     watchers |> Watchers.apply(session, Some(item), log);
                   (
                     {...venture, log, watchers, validation, state, wallet},
                     collector,
                     None,
                   );
                 | PolicyMissmatch as conflict => (
                     venture,
                     collector,
                     Some(Error(venture, item, conflict)),
                   )
                 | PolicyNotFulfilled as conflict => (
                     venture,
                     collector,
                     Some(Error(venture, item, conflict)),
                   )
                 /* Ignored validation issues */
                 | Ignore => (venture, collector, None)
                 | InvalidIssuer =>
                   logMessage("Invalid issuer detected");
                   (venture, collector, None);
                 | BadData(msg) =>
                   logMessage("Bad data in event detected: " ++ msg);
                   (venture, collector, None);
                 | UnknownProcessId =>
                   logMessage("Unknown ProcessId detected");
                   (venture, collector, None);
                 | DependencyNotMet =>
                   logMessage("Dependency Not Met detected");
                   (venture, collector, None);
                 };
               },
             (venture, [], None),
           );
      Js.Promise.(
        watchers
        |> Watchers.processPending(
             session,
             log,
             applyInternal(~syncing=true),
             (validation, state, wallet, collector),
           )
        |> then_(((log, (validation, state, wallet, collector), watchers)) =>
             (
               {...venture, log, validation, state, wallet, watchers},
               collector,
             )
             |> persist
           )
        |> then_(((venture, collector)) =>
             (
               switch (error) {
               | None => Ok(venture, collector)
               | Some(e) => e
               }
             )
             |> resolve
           )
      );
    };
  };
  module SynchronizeWallet = {
    type result =
      | Ok(t, list(Event.t))
      | AlreadyUpToDate;
    let exec = (incomeEvents, venture) => {
      logMessage("Synchronizing wallet");
      Js.Promise.(
        incomeEvents
        |> List.fold_left(
             (p, event) =>
               p
               |> then_(((v, collector)) =>
                    v
                    |> apply(
                         ~systemEvent=true,
                         ~collector,
                         IncomeDetected(event),
                       )
                  ),
             (venture, []) |> resolve,
           )
        |> then_(persist)
        |> then_(((venture, collector)) =>
             (
               switch (collector) {
               | [] => AlreadyUpToDate
               | _ => Ok(venture, collector)
               }
             )
             |> resolve
           )
      );
    };
  };
  module ProposePartner = {
    type result =
      | Ok(t, list(Event.t))
      | PartnerAlreadyExists
      | NoUserInfo;
    let exec = (~prospectId, {session, state} as venture) => {
      logMessage("Executing 'ProposePartner' command");
      if (state |> State.isPartner(prospectId)) {
        PartnerAlreadyExists |> Js.Promise.resolve;
      } else {
        Js.Promise.(
          UserInfo.Public.read(~blockstackId=prospectId)
          |> then_(
               fun
               | UserInfo.Public.Ok(info) => {
                   let partnerProposed =
                     Event.makePartnerProposed(
                       ~supporterId=session.userId,
                       ~prospectId,
                       ~prospectPubKey=info.appPubKey,
                       ~policy=
                         state
                         |> State.currentPolicy(Event.Partner.processName),
                       ~lastRemovalAccepted=
                         state |> State.lastRemovalOfPartner(prospectId),
                     )
                     |> Event.getPartnerProposedExn;
                   let custodianProposal =
                     Event.makeCustodianProposed(
                       ~partnerProposed,
                       ~supporterId=session.userId,
                       ~accountIdx=AccountIndex.default,
                       ~policy=
                         state
                         |> State.currentPolicy(Event.Custodian.processName),
                     )
                     |> Event.getCustodianProposedExn;
                   venture
                   |> apply(Event.PartnerProposed(partnerProposed))
                   |> then_(((v, c)) =>
                        v
                        |> apply(
                             ~collector=c,
                             Event.CustodianProposed(custodianProposal),
                           )
                      )
                   |> then_(persist)
                   |> then_(((v, c)) => resolve(Ok(v, c)));
                 }
               | UserInfo.Public.NotFound => resolve(NoUserInfo),
             )
        );
      };
    };
  };
  module EndorsePartner = {
    type result =
      | Ok(t, list(Event.t));
    let exec = (~processId, {state, session} as venture) => {
      logMessage("Executing 'EndorsePartner' command");
      let custodianProcessId =
        state |> State.custodianProcessForPartnerProcess(processId);
      Js.Promise.(
        venture
        |> apply(
             Event.makePartnerEndorsed(
               ~processId,
               ~supporterId=session.userId,
             ),
           )
        |> then_(((v, c)) =>
             v
             |> apply(
                  ~collector=c,
                  Event.makeCustodianEndorsed(
                    ~processId=custodianProcessId,
                    ~supporterId=session.userId,
                  ),
                )
           )
        |> then_(persist)
        |> then_(((v, c)) => resolve(Ok(v, c)))
      );
    };
  };
  module ProposePartnerRemoval = {
    type result =
      | Ok(t, list(Event.t))
      | PartnerDoesNotExist;
    let exec = (~partnerId, {state, session} as venture) => {
      logMessage("Executing 'ProposePartnerRemoval' command");
      if (state |> State.isPartner(partnerId) == false) {
        PartnerDoesNotExist |> Js.Promise.resolve;
      } else {
        let custodianAccepted =
          state |> State.custodianAcceptedFor(partnerId);
        Js.Promise.(
          venture
          |> apply(
               Event.makeCustodianRemovalProposed(
                 ~custodianAccepted,
                 ~supporterId=session.userId,
                 ~custodianId=partnerId,
                 ~accountIdx=AccountIndex.default,
                 ~policy=
                   state
                   |> State.currentPolicy(Event.Custodian.Removal.processName),
               ),
             )
          |> then_(((v, c)) =>
               v
               |> apply(
                    ~collector=c,
                    Event.makePartnerRemovalProposed(
                      ~supporterId=session.userId,
                      ~partnerId,
                      ~policy=
                        state
                        |> State.currentPolicy(
                             Event.Partner.Removal.processName,
                           ),
                    ),
                  )
             )
          |> then_(persist)
          |> then_(((v, c)) => resolve(Ok(v, c)))
        );
      };
    };
  };
  module EndorsePartnerRemoval = {
    type result =
      | Ok(t, list(Event.t));
    let exec = (~processId, {state, session} as venture) => {
      logMessage("Executing 'EndorsePartnerRemoval' command");
      let custodianRemovalProcessId =
        state
        |> State.custodianRemovalProcessForPartnerRemovalProcess(processId);
      Js.Promise.(
        venture
        |> apply(
             Event.makeCustodianRemovalEndorsed(
               ~processId=custodianRemovalProcessId,
               ~supporterId=session.userId,
             ),
           )
        |> then_(((v, c)) =>
             v
             |> apply(
                  ~collector=c,
                  Event.makePartnerRemovalEndorsed(
                    ~processId,
                    ~supporterId=session.userId,
                  ),
                )
           )
        |> then_(persist)
        |> then_(((v, c)) => resolve(Ok(v, c)))
      );
    };
  };
  module ExposeIncomeAddress = {
    type result =
      | Ok(string, t, list(Event.t));
    let exec = (~accountIdx, {wallet} as venture) => {
      logMessage("Executing 'GetIncomeAddress' command");
      let exposeEvent = wallet |> Wallet.exposeNextIncomeAddress(accountIdx);
      Js.Promise.(
        venture
        |> apply(~systemEvent=true, IncomeAddressExposed(exposeEvent))
        |> then_(persist)
        |> then_(((v, c)) => resolve(Ok(exposeEvent.address, v, c)))
      );
    };
  };
  module ProposePayout = {
    type result =
      | Ok(t, list(Event.t));
    let exec =
        (~accountIdx, ~destinations, ~fee, {wallet, session} as venture) => {
      logMessage("Executing 'ProposePayout' command");
      Js.Promise.(
        Wallet.preparePayoutTx(session, accountIdx, destinations, fee, wallet)
        |> then_(proposal => venture |> apply(PayoutProposed(proposal)))
        |> then_(persist)
        |> then_(((v, c)) => resolve(Ok(v, c)))
      );
    };
  };
  module EndorsePayout = {
    type result =
      | Ok(t, list(Event.t));
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
        |> then_(((v, c)) => resolve(Ok(v, c)))
      );
    };
  };
};
