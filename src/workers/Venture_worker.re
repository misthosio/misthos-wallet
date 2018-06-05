[%bs.raw {| self.localStorage = require("./fakeLocalStorage").localStorage |}];

[%bs.raw
  {| self.window = { localStorage: self.localStorage , location: { origin: self.origin } } |}
];

module Message = VentureWorkerMessage;

type self;

[@bs.val] external self : self = "";

[@bs.set]
external onMessage :
  (self, [@bs.uncurry] ({. "data": WebWorker.message} => unit)) => unit =
  "onmessage";

[@bs.set]
external onError : (self, [@bs.uncurry] ('a => unit)) => unit = "onerror";

[@bs.val] external _postMessage : WebWorker.message => unit = "postMessage";

open PrimitiveTypes;

let postMessage = (~correlationId, msg) =>
  {"payload": msg |> Message.encodeOutgoing, "correlationId": correlationId}
  |> _postMessage;

let logMessage = msg => Js.log("[Venture Worker] - " ++ msg);

let logError = error => {
  Js.Console.error("[Venture Worker] - Encountered an unhandled exception");
  Js.Console.error(error);
};

module Notify = {
  let cmdSuccess = (ventureId, correlationId, response) =>
    postMessage(
      ~correlationId,
      CmdCompleted(ventureId, correlationId, Ok(response)),
    );
  let cmdError = (ventureId, correlationId, response) =>
    postMessage(
      ~correlationId,
      CmdCompleted(ventureId, correlationId, Error(response)),
    );
  let sessionPending = correlationId =>
    postMessage(~correlationId, SessionPending);
  let sessionStarted = (correlationId, blockstackItems, storagePrefix) =>
    postMessage(
      ~correlationId,
      SessionStarted(blockstackItems, storagePrefix),
    );
  let indexUpdated = (correlationId, index) =>
    postMessage(~correlationId, UpdateIndex(index));
  let ventureLoaded = (correlationId, id, venture, newItems) =>
    postMessage(
      ~correlationId,
      VentureLoaded(id, venture |> Venture.getEventLog, newItems),
    );
  let ventureJoined = (correlationId, id, venture) => {
    let log = venture |> Venture.getEventLog;
    postMessage(
      ~correlationId,
      VentureLoaded(id, log, log |> EventLog.items),
    );
  };
  let ventureCreated = venture =>
    postMessage(
      VentureCreated(
        venture |> Venture.getId,
        venture |> Venture.getEventLog,
      ),
    );
  let newIncomeAddress = (correlationId, ventureId, address) =>
    postMessage(~correlationId, NewIncomeAddress(ventureId, address));
  let newItems = (correlationId, id, items) =>
    switch (items) {
    | [||] => ()
    | items => postMessage(~correlationId, NewItems(id, items))
    };
};

type state = {
  venturesThread:
    Js.Promise.t(
      option(
        (Session.Data.t, list((ventureId, Js.Promise.t(Venture.t)))),
      ),
    ),
};

exception DeadThread(Js.Promise.error);

module Handle = {
  type ventureAction =
    | Create(string)
    | Load(ventureId)
    | Reload(ventureId)
    | JoinVia(ventureId, userId);
  let loadAndNotify = (~notify, ~persist=true, data, correlationId, ventureId) =>
    Js.Promise.(
      Venture.load(~persist, data, ~ventureId)
      |> then_(
           fun
           | Venture.Ok(venture, newItems) => {
               if (notify) {
                 Notify.ventureLoaded(
                   correlationId,
                   ventureId,
                   venture,
                   newItems,
                 );
               };
               resolve(venture);
             }
           | Venture.CouldNotLoad(error) => {
               if (notify) {
                 Notify.cmdError(
                   ventureId,
                   correlationId,
                   CouldNotLoadVenture,
                 );
               };
               raise(DeadThread(error));
             },
         )
    );
  let withVenture =
      (~notify=false, ventureAction, f, correlationId, {venturesThread}) => {
    let venturesThread =
      Js.Promise.(
        venturesThread
        |> then_(threads =>
             threads
             |> Utils.mapOption(((data, ventures)) => {
                  let (ventureId, ventureThread) =
                    switch (ventureAction) {
                    | Create(name) =>
                      open Venture.Cmd.Create;
                      let (ventureId, venturePromise) = exec(data, ~name);
                      (
                        ventureId,
                        venturePromise
                        |> then_(
                             fun
                             | Ok(index, venture) => {
                                 Notify.indexUpdated(correlationId, index);
                                 venture |> resolve;
                               }
                             | CouldNotPersist(error) => {
                                 Notify.cmdError(
                                   ventureId,
                                   correlationId,
                                   CouldNotPersistVenture,
                                 );
                                 raise(DeadThread(error));
                               },
                           ),
                      );
                    | Load(ventureId) =>
                      try (
                        ventureId,
                        ventures
                        |> List.assoc(ventureId)
                        |> then_(venture => {
                             if (notify) {
                               Notify.ventureLoaded(
                                 correlationId,
                                 ventureId,
                                 venture,
                                 [||],
                               );
                             };
                             resolve(venture);
                           })
                        |> catch(_ =>
                             loadAndNotify(
                               ~notify,
                               data,
                               correlationId,
                               ventureId,
                             )
                           ),
                      ) {
                      | Not_found => (
                          ventureId,
                          loadAndNotify(
                            ~notify,
                            data,
                            correlationId,
                            ventureId,
                          ),
                        )
                      }
                    | Reload(ventureId) => (
                        ventureId,
                        loadAndNotify(
                          ~notify,
                          ~persist=false,
                          data,
                          correlationId,
                          ventureId,
                        ),
                      )
                    | JoinVia(ventureId, userId) => (
                        ventureId,
                        Venture.join(data, ~userId, ~ventureId)
                        |> then_(
                             fun
                             | Venture.Joined(index, venture) => {
                                 Notify.indexUpdated(correlationId, index);
                                 Notify.ventureJoined(
                                   correlationId,
                                   ventureId,
                                   venture,
                                 );
                                 venture |> resolve;
                               }
                             | Venture.AlreadyLoaded(index, venture, newItems) => {
                                 Notify.indexUpdated(correlationId, index);
                                 Notify.ventureLoaded(
                                   correlationId,
                                   ventureId,
                                   venture,
                                   newItems,
                                 );
                                 venture |> resolve;
                               }
                             | Venture.CouldNotJoin(error) => {
                                 Notify.cmdError(
                                   ventureId,
                                   correlationId,
                                   CouldNotJoinVenture,
                                 );
                                 raise(DeadThread(error));
                               },
                           ),
                      )
                    };
                  (
                    data,
                    [
                      (
                        ventureId,
                        ventureThread
                        |> then_(f(correlationId))
                        |> catch(err => {
                             logError(err);
                             loadAndNotify(
                               ~notify=true,
                               data,
                               correlationId,
                               ventureId,
                             );
                           }),
                      ),
                      ...ventures |> List.remove_assoc(ventureId),
                    ],
                  );
                })
             |> resolve
           )
      );
    {venturesThread: venturesThread};
  };
  let updateSession = (items, correlationId, state) => {
    logMessage("Handling 'UpdateSession'");
    items |> WorkerLocalStorage.setBlockstackItems;
    let sessionThread =
      Js.Promise.(
        Session.getCurrentSession()
        |> then_(
             fun
             | Session.LoggedIn(data) => Some(data) |> resolve
             | _ => None |> resolve,
           )
      );
    Js.Promise.{
      venturesThread:
        all2((sessionThread, state.venturesThread))
        |> then_(((session: option(Session.Data.t), venturesThread)) =>
             switch (session, venturesThread) {
             | (Some(data), Some((oldData: Session.Data.t, threads)))
                 when UserId.eq(data.userId, oldData.userId) =>
               resolve(Some((data, threads)))
             | (Some(data), _) =>
               Notify.sessionStarted(
                 correlationId,
                 items,
                 data.storagePrefix,
               );
               Venture.Index.load()
               |> then_(index =>
                    index |> Notify.indexUpdated(correlationId) |> resolve
                  )
               |> ignore;
               resolve(Some((data, [])));
             | _ =>
               Notify.sessionPending(correlationId);
               resolve(None);
             }
           ),
    };
  };
  let load = ventureId => {
    logMessage("Handling 'Load'");
    withVenture(~notify=true, Load(ventureId), _ => Js.Promise.resolve);
  };
  let joinVia = (ventureId, userId) => {
    logMessage("Handling 'JoinVia'");
    withVenture(JoinVia(ventureId, userId), _ => Js.Promise.resolve);
  };
  let create = name => {
    logMessage("Handling 'Create'");
    withVenture(
      Create(name),
      (correlationId, venture) => {
        Notify.ventureCreated(~correlationId, venture);
        Js.Promise.resolve(venture);
      },
    );
  };
  let proposePartner = (ventureId, prospectId) => {
    logMessage("Handling 'ProposePartner'");
    withVenture(Load(ventureId), (correlationId, venture) =>
      Js.Promise.(
        Venture.Cmd.ProposePartner.(
          venture
          |> exec(~prospectId)
          |> then_(
               fun
               | Ok(processId, venture, newItems) => {
                   Notify.newItems(correlationId, ventureId, newItems);
                   Notify.cmdSuccess(
                     ventureId,
                     correlationId,
                     ProcessStarted(processId),
                   );
                   venture |> resolve;
                 }
               | MaxPartnersReached => {
                   Notify.cmdError(
                     ventureId,
                     correlationId,
                     MaxPartnersReached,
                   );
                   venture |> resolve;
                 }
               | ProposalAlreadyExists => {
                   Notify.cmdError(
                     ventureId,
                     correlationId,
                     PartnerAlreadyProposed,
                   );
                   venture |> resolve;
                 }
               | PartnerAlreadyExists => {
                   Notify.cmdError(
                     ventureId,
                     correlationId,
                     PartnerAlreadyExists,
                   );
                   venture |> resolve;
                 }
               | CouldNotPersist(_err) => {
                   Notify.cmdError(
                     ventureId,
                     correlationId,
                     CouldNotPersistVenture,
                   );
                   venture |> resolve;
                 },
             )
        )
      )
    );
  };
  let rejectPartner = (ventureId, processId) => {
    logMessage("Handling 'RejectPartner'");
    withVenture(Load(ventureId), (correlationId, venture) =>
      Js.Promise.(
        Venture.Cmd.RejectPartner.(
          venture
          |> exec(~processId)
          |> then_(
               fun
               | Ok(venture, newItems) => {
                   Notify.newItems(correlationId, ventureId, newItems);
                   Notify.cmdSuccess(
                     ventureId,
                     correlationId,
                     ProcessRejected(processId),
                   );
                   venture |> resolve;
                 }
               | CouldNotPersist(_err) => {
                   Notify.cmdError(
                     ventureId,
                     correlationId,
                     CouldNotPersistVenture,
                   );
                   venture |> resolve;
                 },
             )
        )
      )
    );
  };
  let endorsePartner = (ventureId, processId) => {
    logMessage("Handling 'EndorsePartner'");
    withVenture(Load(ventureId), (correlationId, venture) =>
      Js.Promise.(
        Venture.Cmd.EndorsePartner.(
          venture
          |> exec(~processId)
          |> then_(
               fun
               | Ok(venture, newItems) => {
                   Notify.newItems(correlationId, ventureId, newItems);
                   Notify.cmdSuccess(
                     ventureId,
                     correlationId,
                     ProcessEndorsed(processId),
                   );
                   venture |> resolve;
                 }
               | CouldNotPersist(_err) => {
                   Notify.cmdError(
                     ventureId,
                     correlationId,
                     CouldNotPersistVenture,
                   );
                   venture |> resolve;
                 },
             )
        )
      )
    );
  };
  let proposePartnerRemoval = (ventureId, partnerId) => {
    logMessage("Handling 'ProposePartnerRemoval'");
    withVenture(Load(ventureId), (correlationId, venture) =>
      Js.Promise.(
        Venture.Cmd.ProposePartnerRemoval.(
          venture
          |> exec(~partnerId)
          |> then_(
               fun
               | Ok(processId, venture, newItems) => {
                   Notify.newItems(correlationId, ventureId, newItems);
                   Notify.cmdSuccess(
                     ventureId,
                     correlationId,
                     ProcessStarted(processId),
                   );
                   venture |> resolve;
                 }
               | PartnerDoesNotExist => venture |> resolve
               | CouldNotPersist(_err) => {
                   Notify.cmdError(
                     ventureId,
                     correlationId,
                     CouldNotPersistVenture,
                   );
                   venture |> resolve;
                 },
             )
        )
      )
    );
  };
  let rejectPartnerRemoval = (ventureId, processId) => {
    logMessage("Handling 'RejectPartnerRemoval'");
    withVenture(Load(ventureId), (correlationId, venture) =>
      Js.Promise.(
        Venture.Cmd.RejectPartnerRemoval.(
          venture
          |> exec(~processId)
          |> then_(
               fun
               | Ok(venture, newItems) => {
                   Notify.newItems(correlationId, ventureId, newItems);
                   Notify.cmdSuccess(
                     ventureId,
                     correlationId,
                     ProcessRejected(processId),
                   );
                   venture |> resolve;
                 }
               | CouldNotPersist(_err) => {
                   Notify.cmdError(
                     ventureId,
                     correlationId,
                     CouldNotPersistVenture,
                   );
                   venture |> resolve;
                 },
             )
        )
      )
    );
  };
  let endorsePartnerRemoval = (ventureId, processId) => {
    logMessage("Handling 'EndorsePartnerRemoval'");
    withVenture(Load(ventureId), (correlationId, venture) =>
      Js.Promise.(
        Venture.Cmd.EndorsePartnerRemoval.(
          venture
          |> exec(~processId)
          |> then_(
               fun
               | Ok(venture, newItems) => {
                   Notify.newItems(correlationId, ventureId, newItems);
                   Notify.cmdSuccess(
                     ventureId,
                     correlationId,
                     ProcessEndorsed(processId),
                   );
                   venture |> resolve;
                 }
               | CouldNotPersist(_err) => {
                   Notify.cmdError(
                     ventureId,
                     correlationId,
                     CouldNotPersistVenture,
                   );
                   venture |> resolve;
                 },
             )
        )
      )
    );
  };
  let proposePayout = (ventureId, accountIdx, destinations, fee) => {
    logMessage("Handling 'ProposePayout'");
    withVenture(Load(ventureId), (correlationId, venture) =>
      Js.Promise.(
        Venture.Cmd.ProposePayout.(
          venture
          |> exec(~accountIdx, ~destinations, ~fee)
          |> then_(
               fun
               | Ok(processId, venture, newItems) => {
                   Notify.newItems(correlationId, ventureId, newItems);
                   Notify.cmdSuccess(
                     ventureId,
                     correlationId,
                     ProcessStarted(processId),
                   );
                   venture |> resolve;
                 }
               | NotEnoughFunds => {
                   logMessage("Not enough funds");
                   venture |> resolve;
                 }
               | CouldNotPersist(_err) => {
                   Notify.cmdError(
                     ventureId,
                     correlationId,
                     CouldNotPersistVenture,
                   );
                   venture |> resolve;
                 },
             )
        )
      )
    );
  };
  let rejectPayout = (ventureId, processId) => {
    logMessage("Handling 'RejectPayout'");
    withVenture(Load(ventureId), (correlationId, venture) =>
      Js.Promise.(
        Venture.Cmd.RejectPayout.(
          venture
          |> exec(~processId)
          |> then_(
               fun
               | Ok(venture, newItems) => {
                   Notify.newItems(correlationId, ventureId, newItems);
                   Notify.cmdSuccess(
                     ventureId,
                     correlationId,
                     ProcessRejected(processId),
                   );
                   venture |> resolve;
                 }
               | CouldNotPersist(_err) => {
                   Notify.cmdError(
                     ventureId,
                     correlationId,
                     CouldNotPersistVenture,
                   );
                   venture |> resolve;
                 },
             )
        )
      )
    );
  };
  let endorsePayout = (ventureId, processId) => {
    logMessage("Handling 'EndorsePayout'");
    withVenture(Load(ventureId), (correlationId, venture) =>
      Js.Promise.(
        Venture.Cmd.EndorsePayout.(
          venture
          |> exec(~processId)
          |> then_(
               fun
               | Ok(venture, newItems) => {
                   Notify.newItems(correlationId, ventureId, newItems);
                   Notify.cmdSuccess(
                     ventureId,
                     correlationId,
                     ProcessEndorsed(processId),
                   );
                   venture |> resolve;
                 }
               | CouldNotPersist(_err) => {
                   Notify.cmdError(
                     ventureId,
                     correlationId,
                     CouldNotPersistVenture,
                   );
                   venture |> resolve;
                 },
             )
        )
      )
    );
  };
  let exposeIncomeAddress = (ventureId, accountIdx) => {
    logMessage("Handling 'ExposeIncomeAddress'");
    withVenture(Load(ventureId), (correlationId, venture) =>
      Js.Promise.(
        Venture.Cmd.ExposeIncomeAddress.(
          venture
          |> exec(~accountIdx)
          |> then_(
               fun
               | Ok(address, venture, newItems) => {
                   Notify.newIncomeAddress(correlationId, ventureId, address);
                   Notify.newItems(correlationId, ventureId, newItems);
                   venture |> resolve;
                 }
               | CouldNotPersist(_err) => {
                   Notify.cmdError(
                     ventureId,
                     correlationId,
                     CouldNotPersistVenture,
                   );
                   venture |> resolve;
                 },
             )
        )
      )
    );
  };
  let syncWallet = (ventureId, broadcasts, broadcastFailures, income, confs) => {
    logMessage("Handling 'SynchWallet'");
    withVenture(Load(ventureId), (correlationId, venture) =>
      Js.Promise.(
        Venture.Cmd.SynchronizeWallet.(
          venture
          |> exec(broadcasts, broadcastFailures, income, confs)
          |> then_(
               fun
               | Ok(venture, newItems) => {
                   Notify.newItems(correlationId, ventureId, newItems);
                   venture |> resolve;
                 }
               | CouldNotPersist(_err) => venture |> resolve,
             )
        )
      )
    );
  };
  let newItemsDetected = (ventureId, items, partnerId) => {
    logMessage("Handling 'NewItemsDetected'");
    withVenture(Load(ventureId), (correlationId, venture) =>
      Js.Promise.(
        Venture.Cmd.SynchronizeLogs.(
          venture
          |> exec(~partnerId, items)
          |> then_(
               fun
               | Ok(venture, newItems) => {
                   Notify.newItems(correlationId, ventureId, newItems);
                   venture |> resolve;
                 }
               | WithConflicts(venture, newItems, conflicts) => {
                   logMessage(
                     "There were "
                     ++ (conflicts |> Array.length |> string_of_int)
                     ++ " conflicts while syncing",
                   );
                   Notify.newItems(correlationId, ventureId, newItems);
                   venture |> resolve;
                 }
               | CouldNotPersist(_err) => venture |> resolve,
             )
        )
      )
    );
  };
  let syncTabs = (ventureId, items) => {
    logMessage("Handling 'SyncTabs'");
    withVenture(~notify=true, Reload(ventureId), (correlationId, venture) =>
      Js.Promise.(
        Venture.Cmd.SynchronizeLogs.(
          venture
          |> exec(items)
          |> then_(
               fun
               | Ok(venture, newItems) => {
                   Notify.newItems(correlationId, ventureId, newItems);
                   venture |> resolve;
                 }
               | WithConflicts(venture, newItems, conflicts) => {
                   logMessage(
                     "There were "
                     ++ (conflicts |> Array.length |> string_of_int)
                     ++ " conflicts while syncing",
                   );
                   Notify.newItems(correlationId, ventureId, newItems);
                   venture |> resolve;
                 }
               | CouldNotPersist(_err) => venture |> resolve,
             )
        )
      )
    );
  };
};

let handleMessage =
  fun
  | Message.UpdateSession(items) => Handle.updateSession(items)
  | Message.Load(ventureId) => Handle.load(ventureId)
  | Message.JoinVia(ventureId, userId) => Handle.joinVia(ventureId, userId)
  | Message.Create(name) => Handle.create(name)
  | Message.ProposePartner(ventureId, userId) =>
    Handle.proposePartner(ventureId, userId)
  | Message.RejectPartner(ventureId, processId) =>
    Handle.rejectPartner(ventureId, processId)
  | Message.EndorsePartner(ventureId, processId) =>
    Handle.endorsePartner(ventureId, processId)
  | Message.ProposePartnerRemoval(ventureId, userId) =>
    Handle.proposePartnerRemoval(ventureId, userId)
  | Message.RejectPartnerRemoval(ventureId, processId) =>
    Handle.rejectPartnerRemoval(ventureId, processId)
  | Message.EndorsePartnerRemoval(ventureId, processId) =>
    Handle.endorsePartnerRemoval(ventureId, processId)
  | Message.ProposePayout(ventureId, accountIdx, destinations, fee) =>
    Handle.proposePayout(ventureId, accountIdx, destinations, fee)
  | Message.RejectPayout(ventureId, processId) =>
    Handle.rejectPayout(ventureId, processId)
  | Message.EndorsePayout(ventureId, processId) =>
    Handle.endorsePayout(ventureId, processId)
  | Message.ExposeIncomeAddress(ventureId, accountIdx) =>
    Handle.exposeIncomeAddress(ventureId, accountIdx)
  | SyncWallet(ventureId, broadcasts, broadcastFailures, income, confs) =>
    Handle.syncWallet(ventureId, broadcasts, broadcastFailures, income, confs)
  | NewItemsDetected(ventureId, items, partnerId) =>
    Handle.newItemsDetected(ventureId, items, partnerId)
  | SyncTabs(ventureId, items) => Handle.syncTabs(ventureId, items);

let cleanState = {venturesThread: Js.Promise.resolve(None)};

let workerState = ref(cleanState);

onMessage(self, msg =>
  workerState :=
    workerState^
    |> handleMessage(
         msg##data##payload |> VentureWorkerMessage.decodeIncoming,
         msg##data##correlationId,
       )
);
