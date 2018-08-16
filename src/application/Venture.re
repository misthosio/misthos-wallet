open PrimitiveTypes;

open WalletTypes;

let logMessage = msg => Js.log("[Venture] - " ++ msg);

module Index = Venture__Index;

module Validation = Venture__Validation;

module State = Venture__State;

exception InvalidEvent(Validation.result);

exception NotPersistingNewEvents;

type t = {
  session: SessionData.t,
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
      ~partnerId,
      issuer,
      event,
      oldLog,
      (validation, state, wallet, collector),
    ) => {
  let (item, log) = oldLog |> EventLog.append(issuer, event);
  switch (item |> Validation.validate(~partnerId, validation)) {
  | Ok =>
    logMessage("Appended event to log:");
    logMessage(Event.encode(event) |> Json.stringify);
    let validation = validation |> Validation.apply(item);
    let state = state |> State.apply(event);
    let wallet = wallet |> Wallet.apply(event);
    let collector = Array.append(collector, [|item|]);
    (Some(item), log, (validation, state, wallet, collector));
  | Ignore => (None, oldLog, (validation, state, wallet, collector))
  | result =>
    logMessage("Event:");
    logMessage(Event.encode(event) |> Json.stringify);
    logMessage("was rejected because of:");
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
      ~collector=[||],
      event,
      {session, id, log, validation, state, wallet, watchers},
    ) => {
  let (item, log, (validation, state, wallet, collector)) =
    applyInternal(
      ~partnerId=session.userId,
      systemEvent ? state |> State.systemIssuer : session.issuerKeyPair,
      event,
      log,
      (validation, state, wallet, collector),
    );
  let (log, (validation, state, wallet, collector), watchers) =
    watchers
    |> Watchers.applyAndProcessPending(
         session,
         item,
         log,
         applyInternal(~partnerId=session.userId),
         (validation, state, wallet, collector),
       );
  ({validation, session, id, log, state, wallet, watchers}, collector);
};

let applyMany = (~collector=[||], venture) =>
  List.fold_left(
    ((v, collector), event) => v |> apply(~collector, event),
    (venture, collector),
  );

let reconstruct = (session, log) => {
  let {validation, state, wallet} = make(session, VentureId.make());
  let (id, validation, state, wallet, _collector, watchers) =
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
           [item, ...collector],
           watchers
           |> Watchers.apply(~reconstruct=true, session, Some(item), log),
         ),
         (VentureId.make(), validation, state, wallet, [], []),
       );
  let (log, (validation, state, wallet, collector), watchers) =
    watchers
    |> Watchers.processPending(
         session,
         log,
         applyInternal(~partnerId=session.userId),
         (validation, state, wallet, [||]),
       );
  ({validation, session, id, log, state, wallet, watchers}, collector);
};

let persist = (~shouldPersist=true, ({id, log} as venture, collector)) =>
  Js.Promise.(
    if (shouldPersist && collector |> Array.length > 0) {
      Blockstack.putFileEncrypted(
        (id |> VentureId.toString) ++ "/log.json",
        log |> EventLog.encode |> Json.stringify,
      )
      |> then_(() => resolve(Js.Result.Ok((venture, collector))))
      |> catch(err => Js.Result.Error(err) |> resolve);
    } else if (collector |> Array.length != 0) {
      raise(NotPersistingNewEvents);
    } else {
      resolve(Js.Result.Ok((venture, collector)));
    }
  );

type loadResult =
  | Ok(t, array(EventLog.item))
  | CouldNotLoad(Js.Promise.error);

let load =
    (~persist as shouldPersist=true, session: SessionData.t, ~ventureId) => {
  logMessage("Loading venture '" ++ VentureId.toString(ventureId) ++ "'");
  Js.Promise.(
    Blockstack.getFileDecrypted(
      (ventureId |> VentureId.toString) ++ "/log.json",
    )
    |> then_(nullLog =>
         (
           switch (Js.Nullable.toOption(nullLog)) {
           | Some(raw) =>
             raw
             |> Json.parseOrRaise
             |> EventLog.decode
             |> reconstruct(session)
           | None => raise(Not_found)
           }
         )
         |> persist(~shouldPersist)
       )
    |> then_(
         fun
         | Js.Result.Ok((v, c)) => Ok(v, c) |> resolve
         | Js.Result.Error(err) => CouldNotLoad(err) |> resolve,
       )
    |> catch(err => CouldNotLoad(err) |> resolve)
  );
};

type joinResult =
  | AlreadyLoaded(Index.t, t, array(EventLog.item))
  | Joined(Index.t, t)
  | CouldNotJoin(Js.Promise.error);

let join = (session: SessionData.t, ~userId, ~ventureId) =>
  Js.Promise.(
    load(session, ~ventureId)
    |> then_(loadResult =>
         switch (loadResult) {
         | Ok(venture, newItems) =>
           Index.add(
             ~ventureId,
             ~ventureName=venture.state |> State.ventureName,
           )
           |> then_(index =>
                AlreadyLoaded(index, venture, newItems) |> resolve
              )
         | CouldNotLoad(_) =>
           Blockstack.getFileFromUserAndDecrypt(
             (ventureId |> VentureId.toString)
             ++ "/"
             ++ session.storagePrefix
             ++ "/log.json",
             ~username=userId |> UserId.toString,
           )
           |> then_(nullFile =>
                (
                  switch (Js.Nullable.toOption(nullFile)) {
                  | None =>
                    logMessage("log file could not be loaded");
                    raise(Not_found);
                  | Some(raw) =>
                    switch (raw |> Json.parse) {
                    | Some(json) =>
                      json |> EventLog.decode |> reconstruct(session)
                    | None =>
                      logMessage("error decoding log");
                      raise(Not_found);
                    }
                  }
                )
                |> persist
              )
           |> then_(
                fun
                | Js.Result.Ok((venture, _)) =>
                  Index.add(
                    ~ventureId=venture.id,
                    ~ventureName=venture.state |> State.ventureName,
                  )
                  |> then_(index => resolve(Joined(index, venture)))
                | Js.Result.Error(err) => CouldNotJoin(err) |> resolve,
              )
           |> catch(err => {
                logMessage("Error while joining");
                Js.log(err);
                CouldNotJoin(err) |> resolve;
              })
         }
       )
  );

let getId = ({id}) => id;

let getSummary = ({log}) => log |> EventLog.getSummary;

let getEventLog = ({log}) => log;

module Cmd = {
  module Create = {
    type result =
      | Ok(Index.t, t)
      | CouldNotPersist(Js.Promise.error);
    let exec =
        (
          session: SessionData.t,
          ~name as ventureName,
          ~defaultAccountSettings,
          ~initialPolicies,
        ) => {
      logMessage("Executing 'Create' command");
      let ventureCreated =
        Event.VentureCreated.make(
          ~ventureName,
          ~creatorId=session.userId,
          ~creatorPubKey=session.issuerKeyPair |> Utils.publicKeyFromKeyPair,
          ~defaultAccountSettings,
          ~metaPolicy=Policy.defaultMetaPolicy,
          ~initialPolicies,
          ~network=session.network,
        );
      (
        ventureCreated.ventureId,
        make(session, ventureCreated.ventureId)
        |> Js.Promise.(
             makeResult =>
               makeResult
               |> apply(VentureCreated(ventureCreated))
               |> persist
               |> then_(
                    fun
                    | Js.Result.Ok((venture, _)) =>
                      Index.add(
                        ~ventureId=venture.id,
                        ~ventureName=venture.state |> State.ventureName,
                      )
                      |> then_(index => resolve(Ok(index, venture)))
                    | Js.Result.Error(err) => CouldNotPersist(err) |> resolve,
                  )
           ),
      );
    };
  };
  module SynchronizeLogs = {
    type result =
      | Ok(t, array(EventLog.item))
      | WithConflicts(
          t,
          array(EventLog.item),
          array((EventLog.item, Validation.result)),
        )
      | CouldNotPersist(Js.Promise.error);
    let exec = (~partnerId=?, newItems, {session} as venture) => {
      let ({log, validation, state, wallet, watchers}, collector, conflicts) =
        newItems
        |> Array.fold_left(
             (
               (
                 {log, watchers, validation, state, wallet} as venture,
                 collector,
                 conflicts,
               ),
               {event} as item: EventLog.item,
             ) =>
               switch (item |> Validation.validate(~partnerId?, validation)) {
               | Ok =>
                 logMessage("Appending synced event to log:");
                 logMessage(Event.encode(event) |> Json.stringify);
                 let log = log |> EventLog.appendItem(item);
                 let validation = validation |> Validation.apply(item);
                 let state = state |> State.apply(event);
                 let wallet = wallet |> Wallet.apply(event);
                 let collector = Array.append(collector, [|item|]);
                 let watchers =
                   watchers |> Watchers.apply(session, Some(item), log);
                 (
                   {...venture, log, watchers, validation, state, wallet},
                   collector,
                   conflicts,
                 );
               | Ignore => (venture, collector, conflicts)
               | conflict =>
                 logMessage(
                   "Encountered '"
                   ++ Validation.resultToString(conflict)
                   ++ "'. Ignoring event:",
                 );
                 logMessage(Event.encode(event) |> Json.stringify);
                 (
                   venture,
                   collector,
                   Array.append(conflicts, [|(item, conflict)|]),
                 );
               },
             (venture, [||], [||]),
           );
      let (log, (validation, state, wallet, collector), watchers) =
        watchers
        |> Watchers.processPending(
             session,
             log,
             applyInternal(~syncing=true, ~partnerId=session.userId),
             (validation, state, wallet, collector),
           );
      Js.Promise.(
        ({...venture, log, validation, state, wallet, watchers}, collector)
        |> persist
        |> then_(
             fun
             | Js.Result.Ok((venture, collector)) =>
               (
                 switch (conflicts) {
                 | [||] => Ok(venture, collector)
                 | conflicts => WithConflicts(venture, collector, conflicts)
                 }
               )
               |> resolve
             | Js.Result.Error(err) => CouldNotPersist(err) |> resolve,
           )
      );
    };
  };
  module SynchronizeWallet = {
    type result =
      | Ok(t, array(EventLog.item))
      | CouldNotPersist(Js.Promise.error);
    let exec =
        (
          broadcasts,
          broadcastFailures,
          incomeEvents,
          unlockedEvents,
          txConfs,
          venture,
        ) => {
      logMessage("Synchronizing wallet");
      Js.Promise.(
        incomeEvents
        |> List.fold_left(
             ((v, collector), event) =>
               v
               |> apply(~systemEvent=true, ~collector, IncomeDetected(event)),
             (venture, [||]),
           )
        |> List.fold_left(
             ((v, collector), event) =>
               v
               |> apply(~systemEvent=true, ~collector, IncomeUnlocked(event)),
             _,
             unlockedEvents,
           )
        |> List.fold_left(
             ((v, collector), event) =>
               v
               |> apply(
                    ~systemEvent=true,
                    ~collector,
                    PayoutBroadcast(event),
                  ),
             _,
             broadcasts,
           )
        |> List.fold_left(
             ((v, collector), event) =>
               v
               |> apply(
                    ~systemEvent=true,
                    ~collector,
                    PayoutBroadcastFailed(event),
                  ),
             _,
             broadcastFailures,
           )
        |> List.fold_left(
             ((v, collector), event) =>
               v
               |> apply(
                    ~systemEvent=true,
                    ~collector,
                    TransactionConfirmed(event),
                  ),
             _,
             txConfs,
           )
        |> persist
        |> then_(
             fun
             | Js.Result.Ok((v, c)) => Ok(v, c) |> resolve
             | Js.Result.Error(err) => CouldNotPersist(err) |> resolve,
           )
      );
    };
  };
  module SubmitCustodianKeyChain = {
    type result =
      | Ok(t, array(EventLog.item))
      | NotACustodian
      | CouldNotPersist(Js.Promise.error);
    let exec = (~keyChain, {session: {userId}, state} as venture) =>
      Js.Promise.(
        switch (state |> State.custodianAcceptedFor(userId)) {
        | None => NotACustodian |> resolve
        | Some((accepted: Event.Custodian.Accepted.t)) =>
          CustodianKeyChainUpdated(
            Event.CustodianKeyChainUpdated.make(
              ~custodianApprovalProcess=accepted.processId,
              ~custodianId=userId,
              ~keyChain,
            ),
          )
          |. apply(venture)
          |> persist
          |> then_(
               fun
               | Js.Result.Ok((v, c)) => Ok(v, c) |> resolve
               | Js.Result.Error(err) => CouldNotPersist(err) |> resolve,
             )
        }
      );
  };
  module ProposePartner = {
    let maxNPartners = 12;
    type result =
      | Ok(processId, t, array(EventLog.item))
      | MaxPartnersReached
      | ProposalAlreadyExists
      | PartnerAlreadyExists
      | UserIdDoesNotExist
      | CouldNotPersist(Js.Promise.error);
    let exec = (~prospectId, {session, state} as venture) => {
      logMessage("Executing 'ProposePartner' command");
      if (state |> State.isPartner(prospectId)) {
        PartnerAlreadyExists |> Js.Promise.resolve;
      } else if (state
                 |> State.currentPartners
                 |> Belt.Set.size >= maxNPartners) {
        MaxPartnersReached |> Js.Promise.resolve;
      } else {
        Js.Promise.(
          Blockstack.lookupProfile(prospectId |> UserId.toString)
          |> then_(_ => UserInfo.Public.read(~blockstackId=prospectId))
          |> then_(
               fun
               | UserInfo.Public.NotFound => None |> resolve
               | UserInfo.Public.Ok(info) => Some(info.appPubKey) |> resolve,
             )
          |> then_(prospectPubKey => {
               let partnerProposed =
                 Event.makePartnerProposed(
                   ~eligibleWhenProposing=state |> State.currentPartners,
                   ~proposerId=session.userId,
                   ~prospectId,
                   ~prospectPubKey?,
                   ~policy=
                     state |> State.currentPolicy(Event.Partner.processName),
                   ~lastRemovalAccepted=
                     state |> State.lastRemovalOfPartner(prospectId),
                   (),
                 )
                 |> Event.getPartnerProposedExn;
               if (state |> State.isPartnerProposalUnique(partnerProposed)) {
                 let custodianProposal =
                   Event.makeCustodianProposed(
                     ~eligibleWhenProposing=state |> State.currentPartners,
                     ~lastCustodianRemovalAccepted=
                       state |> State.lastRemovalOfCustodian(prospectId),
                     ~partnerProposed,
                     ~proposerId=session.userId,
                     ~accountIdx=AccountIndex.default,
                     ~policy=
                       state
                       |> State.currentPolicy(Event.Custodian.processName),
                   )
                   |> Event.getCustodianProposedExn;
                 [
                   Event.PartnerProposed(partnerProposed),
                   Event.makePartnerEndorsed(
                     ~processId=partnerProposed.processId,
                     ~supporterId=session.userId,
                   ),
                   Event.CustodianProposed(custodianProposal),
                   Event.makeCustodianEndorsed(
                     ~processId=custodianProposal.processId,
                     ~supporterId=session.userId,
                   ),
                 ]
                 |> applyMany(venture)
                 |> persist
                 |> then_(
                      fun
                      | Js.Result.Ok((v, c)) =>
                        Ok(partnerProposed.processId, v, c) |> resolve
                      | Js.Result.Error(err) =>
                        CouldNotPersist(err) |> resolve,
                    );
               } else {
                 ProposalAlreadyExists |> resolve;
               };
             })
          |> catch(_ => UserIdDoesNotExist |> resolve)
        );
      };
    };
  };
  module RejectPartner = {
    type result =
      | Ok(t, array(EventLog.item))
      | CouldNotPersist(Js.Promise.error);
    let exec = (~processId, {state, session} as venture) => {
      logMessage("Executing 'RejectPartner' command");
      let custodianProcessId =
        state |> State.custodianProcessForPartnerProcess(processId);
      Js.Promise.(
        [
          Event.makePartnerRejected(~processId, ~rejectorId=session.userId),
          Event.makeCustodianRejected(
            ~processId=custodianProcessId,
            ~rejectorId=session.userId,
          ),
        ]
        |> applyMany(venture)
        |> persist
        |> then_(
             fun
             | Js.Result.Ok((v, c)) => Ok(v, c) |> resolve
             | Js.Result.Error(err) => CouldNotPersist(err) |> resolve,
           )
      );
    };
  };
  module EndorsePartner = {
    type result =
      | Ok(t, array(EventLog.item))
      | CouldNotPersist(Js.Promise.error);
    let exec = (~processId, {state, session} as venture) => {
      logMessage("Executing 'EndorsePartner' command");
      let custodianProcessId =
        state |> State.custodianProcessForPartnerProcess(processId);
      Js.Promise.(
        [
          Event.makePartnerEndorsed(~processId, ~supporterId=session.userId),
          Event.makeCustodianEndorsed(
            ~processId=custodianProcessId,
            ~supporterId=session.userId,
          ),
        ]
        |> applyMany(venture)
        |> persist
        |> then_(
             fun
             | Js.Result.Ok((v, c)) => Ok(v, c) |> resolve
             | Js.Result.Error(err) => CouldNotPersist(err) |> resolve,
           )
      );
    };
  };
  module ProposePartnerRemoval = {
    type result =
      | Ok(processId, t, array(EventLog.item))
      | PartnerDoesNotExist
      | CouldNotPersist(Js.Promise.error);
    let exec = (~partnerId, {state, session} as venture) => {
      logMessage("Executing 'ProposePartnerRemoval' command");
      if (state |> State.isPartner(partnerId) == false) {
        PartnerDoesNotExist |> Js.Promise.resolve;
      } else {
        Js.Promise.(
          (
            switch (state |> State.custodianAcceptedFor(partnerId)) {
            | Some(custodianAccepted) =>
              let custodianRemoval =
                Event.makeCustodianRemovalProposed(
                  ~eligibleWhenProposing=state |> State.currentPartners,
                  ~custodianAccepted,
                  ~proposerId=session.userId,
                  ~accountIdx=AccountIndex.default,
                  ~policy=
                    state
                    |> State.currentPolicy(
                         Event.Custodian.Removal.processName,
                       ),
                )
                |> Event.getCustodianRemovalProposedExn;
              [
                Event.CustodianRemovalProposed(custodianRemoval),
                Event.makeCustodianRemovalEndorsed(
                  ~processId=custodianRemoval.processId,
                  ~supporterId=session.userId,
                ),
              ]
              |> applyMany(venture);
            | None => (venture, [||])
            }
          )
          |> (
            ((v, c)) => {
              let proposal =
                Event.makePartnerRemovalProposed(
                  ~eligibleWhenProposing=state |> State.currentPartners,
                  ~lastPartnerAccepted=
                    state |> State.lastPartnerAccepted(partnerId),
                  ~proposerId=session.userId,
                  ~policy=
                    state
                    |> State.currentPolicy(Event.Partner.Removal.processName),
                )
                |> Event.getPartnerRemovalProposedExn;
              [
                Event.PartnerRemovalProposed(proposal),
                Event.makePartnerRemovalEndorsed(
                  ~processId=proposal.processId,
                  ~supporterId=session.userId,
                ),
              ]
              |> applyMany(~collector=c, v)
              |> persist
              |> then_(
                   fun
                   | Js.Result.Ok((v, c)) =>
                     Ok(proposal.processId, v, c) |> resolve
                   | Js.Result.Error(err) => CouldNotPersist(err) |> resolve,
                 );
            }
          )
        );
      };
    };
  };
  module RejectPartnerRemoval = {
    type result =
      | Ok(t, array(EventLog.item))
      | CouldNotPersist(Js.Promise.error);
    let exec = (~processId, {session} as venture) => {
      logMessage("Executing 'RejectPartnerRemoval' command");
      Js.Promise.(
        venture
        |> apply(
             Event.makePartnerRemovalRejected(
               ~processId,
               ~rejectorId=session.userId,
             ),
           )
        |> persist
        |> then_(
             fun
             | Js.Result.Ok((v, c)) => Ok(v, c) |> resolve
             | Js.Result.Error(err) => CouldNotPersist(err) |> resolve,
           )
      );
    };
  };
  module EndorsePartnerRemoval = {
    type result =
      | Ok(t, array(EventLog.item))
      | CouldNotPersist(Js.Promise.error);
    let exec = (~processId, {state, session} as venture) => {
      logMessage("Executing 'EndorsePartnerRemoval' command");
      Js.Promise.(
        (
          switch (
            state
            |> State.custodianRemovalProcessForPartnerRemovalProcess(
                 processId,
               )
          ) {
          | Some(custodianRemovalProcessId) =>
            venture
            |> apply(
                 Event.makeCustodianRemovalEndorsed(
                   ~processId=custodianRemovalProcessId,
                   ~supporterId=session.userId,
                 ),
               )
          | None => (venture, [||])
          }
        )
        |> (
          ((v, c)) =>
            v
            |> apply(
                 ~collector=c,
                 Event.makePartnerRemovalEndorsed(
                   ~processId,
                   ~supporterId=session.userId,
                 ),
               )
        )
        |> persist
        |> then_(
             fun
             | Js.Result.Ok((v, c)) => Ok(v, c) |> resolve
             | Js.Result.Error(err) => CouldNotPersist(err) |> resolve,
           )
      );
    };
  };
  module ExposeIncomeAddress = {
    type result =
      | Ok(string, t, array(EventLog.item))
      | CouldNotPersist(Js.Promise.error);
    let exec = (~accountIdx, {wallet, session: {userId}} as venture) => {
      logMessage("Executing 'GetIncomeAddress' command");
      let exposeEvent =
        wallet |> Wallet.exposeNextIncomeAddress(userId, accountIdx);
      Js.Promise.(
        venture
        |> apply(IncomeAddressExposed(exposeEvent))
        |> persist
        |> then_(
             fun
             | Js.Result.Ok((v, c)) =>
               Ok(exposeEvent.address.displayAddress, v, c) |> resolve
             | Js.Result.Error(err) => CouldNotPersist(err) |> resolve,
           )
      );
    };
  };
  module ProposePayout = {
    type result =
      | Ok(processId, t, array(EventLog.item))
      | NotEnoughFunds
      | CouldNotPersist(Js.Promise.error);
    let exec =
        (
          ~accountIdx,
          ~payoutTx,
          ~signatures,
          {state, wallet, session} as venture,
        ) => {
      logMessage("Executing 'ProposePayout' command");
      Js.Promise.(
        Wallet.preparePayoutTx(
          ~eligibleWhenProposing=state |> State.currentPartners,
          session,
          accountIdx,
          payoutTx,
          signatures,
          wallet,
        )
        |> (
          fun
          | Wallet.Ok(proposal) =>
            [
              Event.PayoutProposed(proposal),
              Event.makePayoutEndorsed(
                ~processId=proposal.processId,
                ~supporterId=session.userId,
              ),
            ]
            |> applyMany(venture)
            |> persist
            |> then_(
                 fun
                 | Js.Result.Ok((v, c)) =>
                   Ok(proposal.processId, v, c) |> resolve
                 | Js.Result.Error(err) => CouldNotPersist(err) |> resolve,
               )
          | Wallet.NotEnoughFunds => NotEnoughFunds |> resolve
        )
      );
    };
  };
  module RejectPayout = {
    type result =
      | Ok(t, array(EventLog.item))
      | CouldNotPersist(Js.Promise.error);
    let exec = (~processId, {session} as venture) => {
      logMessage("Executing 'RejectPayout' command");
      Js.Promise.(
        venture
        |> apply(
             Event.makePayoutRejected(~processId, ~rejectorId=session.userId),
           )
        |> persist
        |> then_(
             fun
             | Js.Result.Ok((v, c)) => Ok(v, c) |> resolve
             | Js.Result.Error(err) => CouldNotPersist(err) |> resolve,
           )
      );
    };
  };
  module EndorsePayout = {
    type result =
      | Ok(t, array(EventLog.item))
      | CouldNotPersist(Js.Promise.error);
    let exec =
        (
          ~processId,
          ~signatures: array(option((string, string))),
          {session, wallet} as venture,
        ) => {
      logMessage("Executing 'EndorsePayout' command");
      Js.Promise.(
        wallet
        |> Wallet.endorsePayout(processId, signatures, session)
        |> applyMany(venture)
        |> persist
        |> then_(
             fun
             | Js.Result.Ok((v, c)) => Ok(v, c) |> resolve
             | Js.Result.Error(err) => CouldNotPersist(err) |> resolve,
           )
      );
    };
  };
};
