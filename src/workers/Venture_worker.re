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

let logMessage = msg => Js.log("[Venture Worker] - " ++ msg);

type state = {
  sessionData: option(Session.Data.t),
  ventures: list((ventureId, Venture.t)),
};

let state = ref({sessionData: None, ventures: []});

let withSessionData = f =>
  switch (state^.sessionData) {
  | Some(data) => f(data)
  | None => logMessage("Not logged in")
  };

let updateVentureInState = venture =>
  state :=
    {
      ...state^,
      ventures: [
        (venture |> Venture.getId, venture),
        ...state^.ventures |> List.remove_assoc(venture |> Venture.getId),
      ],
    };

let handleMessage =
  fun
  | Message.UpdateSession(items) => {
      logMessage("Updating session in localStorage");
      items |> WorkerLocalStorage.setBlockstackItems;
      Js.Promise.(
        Session.getCurrentSession()
        |> then_(
             fun
             | Session.LoggedIn(data) => {
                 state := {...state^, sessionData: Some(data)};
                 resolve();
               }
             | _ => {
                 state := {sessionData: None, ventures: []};
                 resolve();
               },
           )
      )
      |> ignore;
    }
  | Message.Create(name) =>
    Js.Promise.(
      withSessionData(data =>
        Venture.Cmd.Create.exec(data, ~name)
        |> then_(((index, (venture, events))) => {
             updateVentureInState(venture);
             postMessage(NewEvents(venture |> Venture.getId, events));
             postMessage(UpdateIndex(index)) |> resolve;
           })
        |> ignore
      )
    );

onMessage(self, msg => handleMessage(msg##data));
