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
  lastLoggedInUser: userId,
  ventures: list((ventureId, Venture.t)),
};

let cleanState = {lastLoggedInUser: UserId.fromString(""), ventures: []};

let updateVentureInState = (state, venture) => {
  ...state,
  ventures: [
    (venture |> Venture.getId, venture),
    ...state.ventures |> List.remove_assoc(venture |> Venture.getId),
  ],
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
  let makeSessionPromise = () =>
    Js.Promise.make((~resolve, ~reject as _) =>
      waitForSession(resolve, Js.Promise.resolve(Session.NotLoggedIn))
    );
  let sessionPromise = ref(makeSessionPromise());
  let withSessionData = (f, state) =>
    sessionPromise^ |> Js.Promise.then_(data => f(data, state));
  let updateSession = (items, state) => {
    logMessage("Handling 'UpdateSession'");
    items |> WorkerLocalStorage.setBlockstackItems;
    sessionPromise := makeSessionPromise();
    Js.Promise.(
      Session.getCurrentSession()
      |> then_(
           fun
           | Session.LoggedIn(data) => {
               if (UserId.neq(state.lastLoggedInUser, data.userId)) {
                 Venture.Index.load()
                 |> then_(index => index |> Notify.indexUpdated |> resolve)
                 |> ignore;
               };
               resolve({...state, lastLoggedInUser: data.userId});
             }
           | _ => resolve(cleanState),
         )
    );
  };
  let load = ventureId => {
    logMessage("Handling 'Load'");
    Js.Promise.(
      withSessionData((data, state) =>
        Venture.load(data, ~ventureId)
        |> then_(((venture, events)) => {
             Notify.ventureLoaded(ventureId, events);
             updateVentureInState(state, venture) |> resolve;
           })
      )
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
let workerState = ref(cleanState);

onMessage(self, msg =>
  Js.Promise.(
    resolve()
    |> then_(() =>
         workerState^
         |> handleMessage(msg##data)
         |> then_(newState => {
              workerState := newState;
              resolve();
            })
       )
  )
  |> ignore
);
