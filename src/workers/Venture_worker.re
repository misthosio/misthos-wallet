[%bs.raw {| self.localStorage = require("./fakeLocalStorage").localStorage |}];

[%bs.raw
  {| self.window = { localStorage: self.localStorage , location: { origin: self.origin } } |}
];

module Message = VentureWorkerMessage;

type self;

[@bs.val] external self : self = "";

[@bs.set]
external onMessage :
  (self, [@bs.uncurry] ({. "data": Message.send} => unit)) => unit =
  "onmessage";

[@bs.set]
external onError : (self, [@bs.uncurry] ('a => unit)) => unit = "onerror";

[@bs.val]
external _postMessage : Message.encodedReceive => unit = "postMessage";

open PrimitiveTypes;

module Venture = WorkerizedVenture;

let postMessage = msg => msg |> Message.encodeReceive |> _postMessage;

let logMessage = msg => Js.log("[Venture Worker] - " ++ msg);

module Notify = {
  let indexUpdated = index => postMessage(UpdateIndex(index));
  let ventureLoaded = (id, events) =>
    postMessage(VentureLoaded(id, events |> List.rev));
  let ventureCreated = (id, events) =>
    postMessage(VentureCreated(id, events |> List.rev));
  let newEvents = (id, events) =>
    postMessage(NewEvents(id, events |> List.rev));
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
  let withVenture =
      (~create="", ~ventureId=VentureId.fromString(""), f, {venturesThread}) => {
    let venturesThread =
      Js.Promise.(
        venturesThread
        |> then_(threads =>
             threads
             |> Utils.mapOption(((data, ventures)) => {
                  let (ventureId, ventureThread) =
                    if (create != "") {
                      let (ventureId, venturePromise) =
                        Venture.Cmd.Create.exec(data, ~name=create);
                      (
                        ventureId,
                        venturePromise
                        |> then_(((index, venture)) => {
                             Notify.indexUpdated(index);
                             venture |> resolve;
                           }),
                      );
                    } else {
                      try (ventureId, ventures |> List.assoc(ventureId)) {
                      | Not_found => (
                          ventureId,
                          Venture.load(data, ~ventureId),
                        )
                      };
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
      ~ventureId,
      venture => {
        Notify.ventureLoaded(ventureId, venture |> Venture.getAllEvents);
        Js.Promise.resolve(venture);
      },
    );
  };
  let create = name => {
    logMessage("Handling 'Create'");
    withVenture(
      ~create=name,
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
    withVenture(~ventureId, venture =>
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
};

let handleMessage =
  fun
  | Message.UpdateSession(items) => Handle.updateSession(items)
  | Message.Load(ventureId) => Handle.load(ventureId)
  | Message.Create(name) => Handle.create(name)
  | Message.ProposePartner(ventureId, userId) =>
    Handle.proposePartner(ventureId, userId);

let cleanState = {venturesThread: Js.Promise.resolve(None)};

let workerState = ref(cleanState);

onMessage(self, msg =>
  workerState := workerState^ |> handleMessage(msg##data)
);
