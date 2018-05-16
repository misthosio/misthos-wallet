[%bs.raw {| self.localStorage = require("./fakeLocalStorage").localStorage |}];

[%bs.raw
  {| self.window = { localStorage: self.localStorage , location: { origin: self.origin } } |}
];

module Message = VentureWorkerMessage;

type self;

[@bs.val] external self : self = "";

[@bs.set]
external onMessage :
  (
    self,
    [@bs.uncurry] (
      {. "data": WebWorker.payload(Message.encodedIncoming)} => unit
    )
  ) =>
  unit =
  "onmessage";

[@bs.set]
external onError : (self, [@bs.uncurry] ('a => unit)) => unit = "onerror";

[@bs.val]
external _postMessage : WebWorker.payload(Message.encodedOutgoing) => unit =
  "postMessage";

open PrimitiveTypes;

let postMessage = (~syncId=WebWorker.emptySyncId, msg) =>
  {"msg": msg |> Message.encodeOutgoing, "syncId": syncId} |> _postMessage;

let logMessage = msg => Js.log("[Venture Worker] - " ++ msg);

let logError = error => {
  Js.Console.error("[Venture Worker] - Encountered an unhandled exception");
  Js.Console.error(error);
};

module Notify = {
  let indexUpdated = index => postMessage(UpdateIndex(index));
  let ventureLoaded = (id, venture, newItems) =>
    postMessage(VentureLoaded(id, venture |> Venture.getAllItems, newItems));
  let ventureJoined = (id, venture) => {
    let items = venture |> Venture.getAllItems;
    postMessage(VentureLoaded(id, items, items));
  };
  let ventureCreated = venture =>
    postMessage(
      VentureCreated(
        venture |> Venture.getId,
        venture |> Venture.getAllItems,
      ),
    );
  let newIncomeAddress = (syncId, ventureId, address) =>
    postMessage(~syncId, NewIncomeAddress(ventureId, address));
  let newItems = (id, items) =>
    switch (items) {
    | [||] => ()
    | items => postMessage(NewItems(id, items))
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
  let loadAndNotify = (~notify, ~persist=true, data, ventureId) =>
    Js.Promise.(
      Venture.load(~persist, data, ~ventureId)
      |> then_(
           fun
           | Venture.Ok(venture, newItems) => {
               if (notify) {
                 Notify.ventureLoaded(ventureId, venture, newItems);
               };
               resolve(venture);
             }
           | Venture.CouldNotLoad(error) => raise(DeadThread(error)),
         )
    );
  let withVenture = (~notify=false, ventureAction, f, {venturesThread}) => {
    let venturesThread =
      Js.Promise.(
        venturesThread
        |> then_(threads =>
             threads
             |> Utils.mapOption(((data, ventures)) => {
                  let (ventureId, ventureThread) =
                    switch (ventureAction) {
                    | Create(name) =>
                      let (ventureId, venturePromise) =
                        Venture.Cmd.Create.exec(data, ~name);
                      (
                        ventureId,
                        venturePromise
                        |> then_(((index, venture)) => {
                             Notify.indexUpdated(index);
                             venture |> resolve;
                           }),
                      );
                    | Load(ventureId) =>
                      try (
                        ventureId,
                        ventures
                        |> List.assoc(ventureId)
                        |> then_(venture => {
                             if (notify) {
                               Notify.ventureLoaded(ventureId, venture, [||]);
                             };
                             resolve(venture);
                           })
                        |> catch((_) =>
                             loadAndNotify(~notify, data, ventureId)
                           ),
                      ) {
                      | Not_found => (
                          ventureId,
                          loadAndNotify(~notify, data, ventureId),
                        )
                      }
                    | Reload(ventureId) => (
                        ventureId,
                        loadAndNotify(
                          ~notify,
                          ~persist=false,
                          data,
                          ventureId,
                        ),
                      )
                    | JoinVia(ventureId, userId) => (
                        ventureId,
                        Venture.join(data, ~userId, ~ventureId)
                        |> then_(
                             fun
                             | Venture.Joined(index, venture) => {
                                 Notify.indexUpdated(index);
                                 Notify.ventureJoined(ventureId, venture);
                                 venture |> resolve;
                               }
                             | Venture.AlreadyLoaded(index, venture, newItems) => {
                                 Notify.indexUpdated(index);
                                 Notify.ventureLoaded(
                                   ventureId,
                                   venture,
                                   newItems,
                                 );
                                 venture |> resolve;
                               }
                             | Venture.CouldNotJoin(error) =>
                               raise(DeadThread(error)),
                           ),
                      )
                    };
                  (
                    data,
                    [
                      (
                        ventureId,
                        ventureThread
                        |> then_(f)
                        |> catch(err => {
                             logError(err);
                             loadAndNotify(~notify=true, data, ventureId);
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
  let updateSession = (items, state) => {
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
               Venture.Index.load()
               |> then_(index => index |> Notify.indexUpdated |> resolve)
               |> ignore;
               resolve(Some((data, [])));
             | _ => resolve(None)
             }
           ),
    };
  };
  let load = ventureId => {
    logMessage("Handling 'Load'");
    withVenture(~notify=true, Load(ventureId), Js.Promise.resolve);
  };
  let joinVia = (ventureId, userId) => {
    logMessage("Handling 'JoinVia'");
    withVenture(JoinVia(ventureId, userId), Js.Promise.resolve);
  };
  let create = name => {
    logMessage("Handling 'Create'");
    withVenture(
      Create(name),
      venture => {
        Notify.ventureCreated(venture);
        Js.Promise.resolve(venture);
      },
    );
  };
  let proposePartner = (ventureId, prospectId) => {
    logMessage("Handling 'ProposePartner'");
    withVenture(Load(ventureId), venture =>
      Js.Promise.(
        Venture.Cmd.ProposePartner.(
          venture
          |> exec(~prospectId)
          |> then_(
               fun
               | Ok(venture, newItems) => {
                   Notify.newItems(ventureId, newItems);
                   venture |> resolve;
                 }
               | _ => venture |> resolve,
             )
        )
      )
    );
  };
  let rejectPartner = (ventureId, processId) => {
    logMessage("Handling 'RejectPartner'");
    withVenture(Load(ventureId), venture =>
      Js.Promise.(
        Venture.Cmd.RejectPartner.(
          venture
          |> exec(~processId)
          |> then_(
               fun
               | Ok(venture, newItems) => {
                   Notify.newItems(ventureId, newItems);
                   venture |> resolve;
                 },
             )
        )
      )
    );
  };
  let endorsePartner = (ventureId, processId) => {
    logMessage("Handling 'EndorsePartner'");
    withVenture(Load(ventureId), venture =>
      Js.Promise.(
        Venture.Cmd.EndorsePartner.(
          venture
          |> exec(~processId)
          |> then_(
               fun
               | Ok(venture, newItems) => {
                   Notify.newItems(ventureId, newItems);
                   venture |> resolve;
                 },
             )
        )
      )
    );
  };
  let proposePartnerRemoval = (ventureId, partnerId) => {
    logMessage("Handling 'ProposePartnerRemoval'");
    withVenture(Load(ventureId), venture =>
      Js.Promise.(
        Venture.Cmd.ProposePartnerRemoval.(
          venture
          |> exec(~partnerId)
          |> then_(
               fun
               | Ok(venture, newItems) => {
                   Notify.newItems(ventureId, newItems);
                   venture |> resolve;
                 }
               | _ => venture |> resolve,
             )
        )
      )
    );
  };
  let rejectPartnerRemoval = (ventureId, processId) => {
    logMessage("Handling 'RejectPartnerRemoval'");
    withVenture(Load(ventureId), venture =>
      Js.Promise.(
        Venture.Cmd.RejectPartnerRemoval.(
          venture
          |> exec(~processId)
          |> then_(
               fun
               | Ok(venture, newItems) => {
                   Notify.newItems(ventureId, newItems);
                   venture |> resolve;
                 },
             )
        )
      )
    );
  };
  let endorsePartnerRemoval = (ventureId, processId) => {
    logMessage("Handling 'EndorsePartnerRemoval'");
    withVenture(Load(ventureId), venture =>
      Js.Promise.(
        Venture.Cmd.EndorsePartnerRemoval.(
          venture
          |> exec(~processId)
          |> then_(
               fun
               | Ok(venture, newItems) => {
                   Notify.newItems(ventureId, newItems);
                   venture |> resolve;
                 },
             )
        )
      )
    );
  };
  let proposePayout = (ventureId, accountIdx, destinations, fee) => {
    logMessage("Handling 'ProposePayout'");
    withVenture(Load(ventureId), venture =>
      Js.Promise.(
        Venture.Cmd.ProposePayout.(
          venture
          |> exec(~accountIdx, ~destinations, ~fee)
          |> then_(
               fun
               | Ok(venture, newItems) => {
                   Notify.newItems(ventureId, newItems);
                   venture |> resolve;
                 }
               | NotEnoughFunds => {
                   logMessage("Not enough funds");
                   venture |> resolve;
                 },
             )
        )
      )
    );
  };
  let rejectPayout = (ventureId, processId) => {
    logMessage("Handling 'RejectPayout'");
    withVenture(Load(ventureId), venture =>
      Js.Promise.(
        Venture.Cmd.RejectPayout.(
          venture
          |> exec(~processId)
          |> then_(
               fun
               | Ok(venture, newItems) => {
                   Notify.newItems(ventureId, newItems);
                   venture |> resolve;
                 },
             )
        )
      )
    );
  };
  let endorsePayout = (ventureId, processId) => {
    logMessage("Handling 'EndorsePayout'");
    withVenture(Load(ventureId), venture =>
      Js.Promise.(
        Venture.Cmd.EndorsePayout.(
          venture
          |> exec(~processId)
          |> then_(
               fun
               | Ok(venture, newItems) => {
                   Notify.newItems(ventureId, newItems);
                   venture |> resolve;
                 },
             )
        )
      )
    );
  };
  let exposeIncomeAddress = (syncId, ventureId, accountIdx) => {
    logMessage("Handling 'ExposeIncomeAddress'");
    withVenture(Load(ventureId), venture =>
      Js.Promise.(
        Venture.Cmd.ExposeIncomeAddress.(
          venture
          |> exec(~accountIdx)
          |> then_(
               fun
               | Ok(address, venture, newItems) => {
                   Notify.newIncomeAddress(syncId, ventureId, address);
                   Notify.newItems(ventureId, newItems);
                   venture |> resolve;
                 },
             )
        )
      )
    );
  };
  let syncWallet = (ventureId, events, confs) => {
    logMessage("Handling 'SynchWallet'");
    withVenture(Load(ventureId), venture =>
      Js.Promise.(
        Venture.Cmd.SynchronizeWallet.(
          venture
          |> exec(events, confs)
          |> then_(
               fun
               | Ok(venture, newItems) => {
                   Notify.newItems(ventureId, newItems);
                   venture |> resolve;
                 },
             )
        )
      )
    );
  };
  let newItemsDetected = (ventureId, items) => {
    logMessage("Handling 'NewItemsDetected'");
    withVenture(Load(ventureId), venture =>
      Js.Promise.(
        Venture.Cmd.SynchronizeLogs.(
          venture
          |> exec(items)
          |> then_(
               fun
               | Ok(venture, newItems) => {
                   Notify.newItems(ventureId, newItems);
                   venture |> resolve;
                 }
               | WithConflicts(venture, newItems, conflicts) => {
                   logMessage(
                     "There were "
                     ++ (conflicts |> Array.length |> string_of_int)
                     ++ " conflicts while syncing",
                   );
                   Notify.newItems(ventureId, newItems);
                   venture |> resolve;
                 },
             )
        )
      )
    );
  };
  let syncTabs = (ventureId, items) => {
    logMessage("Handling 'SyncTabs'");
    withVenture(~notify=true, Reload(ventureId), venture =>
      Js.Promise.(
        Venture.Cmd.SynchronizeLogs.(
          venture
          |> exec(items)
          |> then_(
               fun
               | Ok(venture, newItems) => {
                   Notify.newItems(ventureId, newItems);
                   venture |> resolve;
                 }
               | WithConflicts(venture, newItems, conflicts) => {
                   logMessage(
                     "There were "
                     ++ (conflicts |> Array.length |> string_of_int)
                     ++ " conflicts while syncing",
                   );
                   Notify.newItems(ventureId, newItems);
                   venture |> resolve;
                 },
             )
        )
      )
    );
  };
};

let handleMessage = syncId =>
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
    Handle.exposeIncomeAddress(syncId, ventureId, accountIdx)
  | SyncWallet(ventureId, income, confs) =>
    Handle.syncWallet(ventureId, income, confs)
  | NewItemsDetected(ventureId, items) =>
    Handle.newItemsDetected(ventureId, items)
  | SyncTabs(ventureId, items) => Handle.syncTabs(ventureId, items);

let cleanState = {venturesThread: Js.Promise.resolve(None)};

let workerState = ref(cleanState);

onMessage(self, msg =>
  workerState :=
    workerState^
    |> handleMessage(
         msg##data##syncId,
         msg##data##msg |> VentureWorkerMessage.decodeIncoming,
       )
);
