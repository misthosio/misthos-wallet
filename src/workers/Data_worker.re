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

open Belt;

open PrimitiveTypes;

open VentureWorkerMessage;

let logLabel = "[Data Worker]";

let logMessage = WorkerUtils.logMessage(logLabel);

let catchAndLogError = WorkerUtils.catchAndLogError(logLabel);

let tenSecondsInMilliseconds = 10000;

let syncInterval = tenSecondsInMilliseconds;

let handleMsg = (venturesPromise, doWork, msg) =>
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
                |> then_(ventures => {
                     let ventures =
                       ventures |> Map.mergeMany(VentureId.makeMap());
                     doWork(ventures);
                     ventures |> resolve;
                   })
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

onMessage(
  self,
  msg => {
    let doWork = ventures => IncomeCollection.doWork(ventures);
    venturesPromise :=
      handleMsg(
        venturesPromise^,
        doWork,
        msg##data##msg |> DataWorkerMessage.decodeIncoming,
      );
    intervalId :=
      (
        switch (intervalId^) {
        | None =>
          Some(
            Js.Global.setInterval(
              () =>
                Js.Promise.(
                  venturesPromise^
                  |> then_(ventures => doWork(ventures) |> resolve)
                  |> catchAndLogError
                ),
              syncInterval,
            ),
          )
        | id => id
        }
      );
  },
);
