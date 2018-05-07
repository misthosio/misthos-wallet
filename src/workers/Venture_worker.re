[%bs.raw {| self.localStorage = require("./fakeLocalStorage").localStorage |}];

[%bs.raw
  {| self.window = { localStorage: self.localStorage , location: { origin: self.origin } } |}
];

module Message = VentureWorkerMessage;

type self;

[@bs.val] external self : self = "";

[@bs.set]
external onMessage :
  (self, [@bs.uncurry] ({. "data": Message.incoming} => unit)) => unit =
  "onmessage";

[@bs.set]
external onError : (self, [@bs.uncurry] ('a => unit)) => unit = "onerror";

[@bs.val]
external _postMessage : Message.encodedOutgoing => unit = "postMessage";

open PrimitiveTypes;

let postMessage = msg => msg |> Message.encodeOutgoing |> _postMessage;

let logMessage = msg => Js.log("[Venture Worker] - " ++ msg);

module Notify = {
  let indexUpdated = index => postMessage(UpdateIndex(index));
  let ventureLoaded = (id, events) =>
    postMessage(VentureLoaded(id, events |> List.rev));
  let ventureCreated = (id, events) =>
    postMessage(VentureCreated(id, events |> List.rev));
  let newEvents = (id, events) =>
    switch (events) {
    | [] => ()
    | events => postMessage(NewEvents(id, events |> List.rev))
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

module Handle = {
  type ventureAction =
    | Create(string)
    | Load(ventureId)
    | JoinVia(ventureId, userId);
  let withVenture = (ventureAction, f, {venturesThread}) => {
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
                      try (ventureId, ventures |> List.assoc(ventureId)) {
                      | Not_found => (
                          ventureId,
                          Venture.load(data, ~ventureId),
                        )
                      }
                    | JoinVia(ventureId, userId) => (
                        ventureId,
                        Venture.join(data, ~userId, ~ventureId)
                        |> then_(((index, venture)) => {
                             Notify.indexUpdated(index);
                             venture |> resolve;
                           }),
                      )
                    };
                  (
                    data,
                    [
                      (ventureId, ventureThread |> then_(f)),
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
    withVenture(
      Load(ventureId),
      venture => {
        Notify.ventureLoaded(ventureId, venture |> Venture.getAllEvents);
        Js.Promise.resolve(venture);
      },
    );
  };
  let joinVia = (ventureId, userId) => {
    logMessage("Handling 'JoinVia'");
    withVenture(
      JoinVia(ventureId, userId),
      venture => {
        Notify.ventureLoaded(ventureId, venture |> Venture.getAllEvents);
        Js.Promise.resolve(venture);
      },
    );
  };
  let create = name => {
    logMessage("Handling 'Create'");
    withVenture(
      Create(name),
      venture => {
        Notify.ventureCreated(
          venture |> Venture.getId,
          venture |> Venture.getAllEvents,
        );
        Js.Promise.resolve(venture);
      },
    );
  };
  let proposePartner = (ventureId, prospectId) => {
    logMessage("Handing 'ProposePartner'");
    withVenture(Load(ventureId), venture =>
      Js.Promise.(
        Venture.Cmd.ProposePartner.(
          venture
          |> exec(~prospectId)
          |> then_(
               fun
               | Ok(venture, newEvents) => {
                   Notify.newEvents(ventureId, newEvents);
                   venture |> resolve;
                 }
               | _ => venture |> resolve,
             )
        )
      )
    );
  };
  let endorsePartner = (ventureId, processId) => {
    logMessage("Handing 'EndorsePartner'");
    withVenture(Load(ventureId), venture =>
      Js.Promise.(
        Venture.Cmd.EndorsePartner.(
          venture
          |> exec(~processId)
          |> then_(
               fun
               | Ok(venture, newEvents) => {
                   Notify.newEvents(ventureId, newEvents);
                   venture |> resolve;
                 },
             )
        )
      )
    );
  };
  let proposePartnerRemoval = (ventureId, partnerId) => {
    logMessage("Handing 'ProposePartnerRemoval'");
    withVenture(Load(ventureId), venture =>
      Js.Promise.(
        Venture.Cmd.ProposePartnerRemoval.(
          venture
          |> exec(~partnerId)
          |> then_(
               fun
               | Ok(venture, newEvents) => {
                   Notify.newEvents(ventureId, newEvents);
                   venture |> resolve;
                 }
               | _ => venture |> resolve,
             )
        )
      )
    );
  };
  let endorsePartnerRemoval = (ventureId, processId) => {
    logMessage("Handing 'EndorsePartnerRemoval'");
    withVenture(Load(ventureId), venture =>
      Js.Promise.(
        Venture.Cmd.EndorsePartnerRemoval.(
          venture
          |> exec(~processId)
          |> then_(
               fun
               | Ok(venture, newEvents) => {
                   Notify.newEvents(ventureId, newEvents);
                   venture |> resolve;
                 },
             )
        )
      )
    );
  };
  let proposePayout = (ventureId, accountIdx, destinations, fee) => {
    logMessage("Handing 'ProposePayout'");
    withVenture(Load(ventureId), venture =>
      Js.Promise.(
        Venture.Cmd.ProposePayout.(
          venture
          |> exec(~accountIdx, ~destinations, ~fee)
          |> then_(
               fun
               | Ok(venture, newEvents) => {
                   Notify.newEvents(ventureId, newEvents);
                   venture |> resolve;
                 },
             )
        )
      )
    );
  };
  let endorsePayout = (ventureId, processId) => {
    logMessage("Handing 'EndorsePayout'");
    withVenture(Load(ventureId), venture =>
      Js.Promise.(
        Venture.Cmd.EndorsePayout.(
          venture
          |> exec(~processId)
          |> then_(
               fun
               | Ok(venture, newEvents) => {
                   Notify.newEvents(ventureId, newEvents);
                   venture |> resolve;
                 },
             )
        )
      )
    );
  };
  let exposeIncomeAddress = (ventureId, accountIdx) => {
    logMessage("Handing 'ExposeIncomeAddress'");
    withVenture(Load(ventureId), venture =>
      Js.Promise.(
        Venture.Cmd.ExposeIncomeAddress.(
          venture
          |> exec(~accountIdx)
          |> then_(
               fun
               | Ok(_address, venture, newEvents) => {
                   Notify.newEvents(ventureId, newEvents);
                   venture |> resolve;
                 },
             )
        )
      )
    );
  };
  let transactionDetected = (ventureId, events) => {
    logMessage("Handing 'TransactionDetected'");
    withVenture(Load(ventureId), venture =>
      Js.Promise.(
        Venture.Cmd.SynchronizeWallet.(
          venture
          |> exec(events)
          |> then_(
               fun
               | Ok(venture, newEvents) => {
                   Notify.newEvents(ventureId, newEvents);
                   venture |> resolve;
                 },
             )
        )
      )
    );
  };
  let newItemsDetected = (ventureId, items) => {
    logMessage("Handing 'NewItemsDetected'");
    withVenture(Load(ventureId), venture =>
      Js.Promise.(
        Venture.Cmd.SynchronizeLogs.(
          venture
          |> exec(items)
          |> then_(
               fun
               | Ok(venture, newEvents) => {
                   Notify.newEvents(ventureId, newEvents);
                   venture |> resolve;
                 }
               | WithConflicts(venture, newEvents, conflicts) => {
                   logMessage(
                     "There were "
                     ++ (conflicts |> List.length |> string_of_int)
                     ++ " conflicts while syncing",
                   );
                   Notify.newEvents(ventureId, newEvents);
                   venture |> resolve;
                 },
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
  | Message.EndorsePartner(ventureId, processId) =>
    Handle.endorsePartner(ventureId, processId)
  | Message.ProposePartnerRemoval(ventureId, userId) =>
    Handle.proposePartnerRemoval(ventureId, userId)
  | Message.EndorsePartnerRemoval(ventureId, processId) =>
    Handle.endorsePartnerRemoval(ventureId, processId)
  | Message.ProposePayout(ventureId, accountIdx, destinations, fee) =>
    Handle.proposePayout(
      ventureId,
      accountIdx,
      destinations |> List.map(((a, btc)) => (a, btc |> BTC.decode)),
      fee |> BTC.decode,
    )
  | Message.EndorsePayout(ventureId, processId) =>
    Handle.endorsePayout(ventureId, processId)
  | Message.ExposeIncomeAddress(ventureId, accountIdx) =>
    Handle.exposeIncomeAddress(ventureId, accountIdx)
  | TransactionDetected(ventureId, events) =>
    Handle.transactionDetected(
      ventureId,
      events |> List.map(Event.IncomeDetected.decode),
    )
  | NewItemsDetected(ventureId, items) =>
    Handle.newItemsDetected(
      ventureId,
      items |> List.map(EventLog.decodeItem),
    );

let cleanState = {venturesThread: Js.Promise.resolve(None)};

let workerState = ref(cleanState);

onMessage(self, msg =>
  workerState := workerState^ |> handleMessage(msg##data)
);