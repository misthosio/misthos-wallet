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

let postMessage = msg => msg |> Message.encodeReceive |> _postMessage;

open PrimitiveTypes;

module Venture = WorkerizedVenture;

module Notify = {
  let indexUpdated = index => postMessage(UpdateIndex(index));
  let ventureLoaded = (id, events) =>
    postMessage(VentureLoaded(id, events |> List.rev));
};

let logMessage = msg => Js.log("[Venture Worker] - " ++ msg);

type state = {
  venturesThread:
    Js.Promise.t(
      option(
        (Session.Data.t, list((ventureId, Js.Promise.t(Venture.t)))),
      ),
    ),
};

module Handle = {
  let rec waitForSession = (resolvePromise: (. 'a) => unit, sessionPromise) =>
    Js.Promise.(
      sessionPromise
      |> then_(
           fun
           | Session.LoggedIn(data) => resolvePromise(. data) |> resolve
           | _ =>
             Js.Global.setTimeout(
               () =>
                 waitForSession(resolvePromise, Session.getCurrentSession()),
               200,
             )
             |> ignore
             |> resolve,
         )
    )
    |> ignore;
  let withVenture = (ventureId, f, {venturesThread}) => {
    let venturesThread =
      Js.Promise.(
        venturesThread
        |> then_(threads =>
             threads
             |> Utils.mapOption(((data, ventures)) => {
                  let ventureThread =
                    try (ventures |> List.assoc(ventureId)) {
                    | Not_found => Venture.load(data, ~ventureId)
                    };
                  (
                    data,
                    [
                      (
                        ventureId,
                        ventureThread |> then_(venture => f(data, venture)),
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
    withVenture(
      ventureId,
      (_session, venture) => {
        Notify.ventureLoaded(ventureId, venture |> Venture.getAllEvents);
        Js.Promise.resolve(venture);
      },
    );
  };
};

let handleMessage =
  fun
  | Message.UpdateSession(items) => Handle.updateSession(items)
  | Message.Load(ventureId) => Handle.load(ventureId);

/* | Message.Create(name) => */
/*   Js.Promise.( */
/*     withSessionData((data, state) => */
/*       Venture.Cmd.Create.exec(data, ~name) */
/*       |> then_(((index, (venture, events))) => { */
/*            let newState = updateVentureInState(state, venture); */
/*            postMessage(NewEvents(venture |> Venture.getId, events)); */
/*            index |> Notify.indexUpdated; */
/*            newState |> resolve; */
/*          }) */
/*     ) */
/*   ); */
let cleanState = {venturesThread: Js.Promise.resolve(None)};

let workerState = ref(cleanState);

onMessage(self, msg =>
  workerState := workerState^ |> handleMessage(msg##data)
);
