open PrimitiveTypes;

open WalletTypes;

let logMessage = msg => Js.log("[Venture] - " ++ msg);

module Index = Venture__Index;

module Validation = Venture__Validation;

exception InvalidEvent(Validation.result);

exception CouldNotLoadVenture;

type t = {
  session: Session.Data.t,
  id: ventureId,
  log: EventLog.t,
  state: Validation.state,
  wallet: Venture__Wallet.t,
  watchers: Watchers.t,
};

module Wallet = Venture__Wallet;

let make = (session, id) => {
  session,
  id,
  log: EventLog.make(),
  state: Validation.makeState(),
  wallet: Wallet.make(),
  watchers: [],
};

let applyInternal =
    (~syncing=false, issuer, event, oldLog, (state, wallet, collector)) => {
  let (item, log) = oldLog |> EventLog.append(issuer, event);
  switch (item |> Validation.validate(state)) {
  | Ok =>
    logMessage("Appended event to log:");
    logMessage(Event.encode(event) |> Json.stringify);
    let state = state |> Validation.apply(event);
    let wallet = wallet |> Wallet.apply(event);
    let collector = [event, ...collector];
    (Some(item), log, (state, wallet, collector));
  | Ignore =>
    logMessage("Ignoring event:");
    logMessage(Event.encode(event) |> Json.stringify);
    (None, oldLog, (state, wallet, collector));
  /* This should never happen / only incase of an UI input bug!!! */
  | result =>
    logMessage("Event was rejected because of:");
    logMessage(Validation.resultToString(result));
    syncing ?
      (None, oldLog, (state, wallet, collector)) :
      raise(InvalidEvent(result));
  };
};

let apply =
    (
      ~systemEvent=false,
      ~collector=[],
      event,
      {session, id, log, state, wallet, watchers},
    ) => {
  let (item, log, (state, wallet, collector)) =
    applyInternal(
      systemEvent ? state.systemIssuer : session.issuerKeyPair,
      event,
      log,
      (state, wallet, collector),
    );
  Js.Promise.(
    watchers
    |> Watchers.applyAndProcessPending(
         session,
         item,
         log,
         applyInternal,
         (state, wallet, collector),
       )
    |> then_(((log, (state, wallet, collector), watchers)) =>
         ({session, id, log, state, wallet, watchers}, collector) |> resolve
       )
  );
};

let reconstruct = (session, log) => {
  let {state, wallet} = make(session, VentureId.make());
  let (id, state, wallet, collector, watchers) =
    log
    |> EventLog.reduce(
         ((id, state, wallet, collector, watchers), {event} as item) => (
           switch (event) {
           | VentureCreated({ventureId}) => ventureId
           | _ => id
           },
           state |> Validation.apply(event),
           wallet |> Wallet.apply(event),
           [event, ...collector],
           watchers
           |> Watchers.apply(~reconstruct=true, session, Some(item), log),
         ),
         (VentureId.make(), state, wallet, [], []),
       );
  watchers
  |> Watchers.processPending(
       session,
       log,
       applyInternal,
       (state, wallet, collector),
     )
  |> Js.Promise.then_(((log, (state, wallet, collector), watchers)) =>
       ({session, id, log, state, wallet, watchers}, collector)
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
                ~ventureName=venture.state.ventureName,
              )
              |> then_(index => resolve((index, venture)))
            )
       )
  );

let getId = ({id}) => id;

let getSummary = ({log}) => log |> EventLog.getSummary;

let getAllEvents = ({log}) =>
  log |> EventLog.reduce((l, {event}) => [event, ...l], []);

module SynchronizeLogs = {
  type result =
    | Ok(t, list(Event.t))
    | Error(t, EventLog.item, Validation.result);
  let exec = (newItems, {session} as venture) => {
    let ({log, state, wallet, watchers}, collector, error) =
      newItems
      |> List.fold_left(
           (
             ({log, watchers, state, wallet} as venture, collector, error),
             {event} as item: EventLog.item,
           ) =>
             if (Js.Option.isSome(error)) {
               (venture, collector, error);
             } else {
               switch (item |> Validation.validate(state)) {
               | Ok =>
                 logMessage("Appending synced event to log:");
                 logMessage(Event.encode(event) |> Json.stringify);
                 let log = log |> EventLog.appendItem(item);
                 let state = state |> Validation.apply(event);
                 let wallet = wallet |> Wallet.apply(event);
                 let collector = [event, ...collector];
                 let watchers =
                   watchers |> Watchers.apply(session, Some(item), log);
                 (
                   {...venture, log, watchers, state, wallet},
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
           (state, wallet, collector),
         )
      |> then_(((log, (state, wallet, collector), watchers)) =>
           ({...venture, log, state, wallet, watchers}, collector) |> persist
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
  module SynchronizeLogs = SynchronizeLogs;
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
      if (state.partnerIds |> List.mem(prospectId)) {
        PartnerAlreadyExists |> Js.Promise.resolve;
      } else {
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
                         state.policies
                         |> List.assoc(Event.Partner.processName),
                     )
                     |> Event.getPartnerProposedExn;
                   let custodianProposal =
                     Event.makeCustodianProposed(
                       ~partnerApprovalProcess=partnerProposal.processId,
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
      if (state.partnerIds |> List.mem(partnerId) == false) {
        PartnerDoesNotExist |> Js.Promise.resolve;
      } else {
        let (custodianProcess, _) =
          state.custodianData
          |> List.find(
               ((_pId, {partnerId: custodianId}: Event.Custodian.Data.t)) =>
               UserId.eq(partnerId, custodianId)
             );
        Js.Promise.(
          venture
          |> apply(
               Event.makeCustodianRemovalProposed(
                 ~dependsOn=Some(custodianProcess),
                 ~supporterId=session.userId,
                 ~custodianId=partnerId,
                 ~accountIdx=AccountIndex.default,
                 ~policy=
                   state.policies
                   |> List.assoc(Event.Custodian.Removal.processName),
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
                        state.policies
                        |> List.assoc(Event.Partner.Removal.processName),
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
      let {id: partnerId}: Event.Partner.Removal.Data.t =
        state.partnerRemovalData |> List.assoc(processId);
      let (custodianProcessId, _) =
        state.custodianRemovalData
        |> List.find(((_, {custodianId}: Event.Custodian.Removal.Data.t)) =>
             custodianId == partnerId
           );
      Js.Promise.(
        venture
        |> apply(
             Event.makeCustodianRemovalEndorsed(
               ~processId=custodianProcessId,
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
