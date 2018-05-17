[%bs.raw {| self.localStorage = require("./fakeLocalStorage").localStorage |}];

[%bs.raw
  {| self.window = { localStorage: self.localStorage , location: { origin: self.origin } } |}
];

type self;

[@bs.val] external self : self = "";

[@bs.set]
external onMessage :
  (self, [@bs.uncurry] ({. "data": WebWorker.payload} => unit)) => unit =
  "onmessage";

[@bs.val] external _postMessage : WebWorker.payload => unit = "postMessage";

open Belt;

open PrimitiveTypes;

open VentureWorkerMessage;

let postMessage = msg =>
  {
    "msg": msg |> VentureWorkerMessage.encodeIncoming,
    "syncId": WebWorker.emptySyncId,
  }
  |> _postMessage;

let logMessage = msg => Js.log("[Data Worker] - " ++ msg);

let tenSecondsInMilliseconds = 10000;

let syncInterval = tenSecondsInMilliseconds;

let handleMsg = (venturesPromise, msg) =>
  Js.Promise.(
    venturesPromise
    |> then_(ventures =>
         switch (msg) {
         | SessionPending =>
           logMessage("Handling 'SessionPending'");
           VentureId.makeMap() |> resolve;
         | SessionStarted(items) =>
           logMessage("Handling 'SessionStarted'");
           items |> WorkerLocalStorage.setBlockstackItems;
           Venture.Index.load()
           |> then_(index =>
                all(
                  index
                  |. List.map(({id}: Venture.Index.item) =>
                       WorkerUtils.loadVenture(id)
                       |> then_(venture => (id, venture) |> resolve)
                     )
                  |> List.toArray,
                )
                |> then_(ventures =>
                     ventures |> Map.mergeMany(VentureId.makeMap()) |> resolve
                   )
              );
         | VentureLoaded(ventureId, log, _) =>
           logMessage("Handling 'VentureLoaded'");
           ventures |. Map.set(ventureId, log) |> resolve;
         | VentureCreated(ventureId, log) =>
           logMessage("Handling 'VentureCreated'");
           ventures |. Map.set(ventureId, log) |> resolve;
         | NewItems(ventureId, items) =>
           logMessage("Handling 'NewItems'");
           let venture = ventures |. Map.getExn(ventureId);
           ventures
           |. Map.set(ventureId, venture |> EventLog.appendItems(items))
           |> resolve;
         | NewIncomeAddress(_, _)
         | UpdateIndex(_) => ventures |> resolve
         }
       )
  );

let intervalId: ref(option(Js.Global.intervalId)) = ref(None);

let venturesPromise: ref(Js.Promise.t(VentureId.map(EventLog.t))) =
  ref(VentureId.makeMap() |> Js.Promise.resolve);

onMessage(self, msg =>
  venturesPromise :=
    handleMsg(
      venturesPromise^,
      msg##data##msg |> DataWorkerMessage.decodeIncoming,
    )
);
/* intervalId^ */
/* |> Utils.mapOption(id => */
/*      if (newIntervalid != id) { */
/*        Js.Global.clearInterval(id); */
/*      } */
/*    ) */
/* |> ignore; */
/* intervalId := Some(newIntervalid); */
