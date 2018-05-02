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
};

let logMessage = msg => Js.log("[Venture Worker] - " ++ msg);

type state = {
  sessionData: option(Session.Data.t),
  ventures: list((ventureId, Venture.t)),
};

let cleanState = {sessionData: None, ventures: []};

let withSessionData = (f, state) =>
  Js.Promise.(
    switch (state.sessionData) {
    | Some(data) => f(data, state)
    | None => resolve(state)
    }
  );

let updateVentureInState = (state, venture) => {
  ...state,
  ventures: [
    (venture |> Venture.getId, venture),
    ...state.ventures |> List.remove_assoc(venture |> Venture.getId),
  ],
};

module Handle = {
  let updateSession = (items, state) => {
    items |> WorkerLocalStorage.setBlockstackItems;
    Js.Promise.(
      Session.getCurrentSession()
      |> then_(
           fun
           | Session.LoggedIn(data) => {
               let oldData = state.sessionData;
               switch (oldData) {
               | None =>
                 Venture.Index.load()
                 |> then_(index => index |> Notify.indexUpdated |> resolve)
                 |> ignore
               | Some(oldData) when UserId.neq(oldData.userId, data.userId) =>
                 Venture.Index.load()
                 |> then_(index => index |> Notify.indexUpdated |> resolve)
                 |> ignore
               | _ => ()
               };
               resolve({...state, sessionData: Some(data)});
             }
           | _ => resolve(cleanState),
         )
    );
  };
};

let handleMessage =
  fun
  | Message.UpdateSession(items) => Handle.updateSession(items)
  | Message.Create(name) =>
    Js.Promise.(
      withSessionData((data, state) =>
        Venture.Cmd.Create.exec(data, ~name)
        |> then_(((index, (venture, events))) => {
             let newState = updateVentureInState(state, venture);
             postMessage(NewEvents(venture |> Venture.getId, events));
             index |> Notify.indexUpdated;
             newState |> resolve;
           })
      )
    );

let workerState = ref(cleanState);

onMessage(self, msg =>
  Js.Promise.(
    workerState^
    |> handleMessage(msg##data)
    |> then_(newState => {
         workerState := newState;
         resolve();
       })
  )
  |> ignore
);
