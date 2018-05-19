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

let logError = WorkerUtils.logError(logLabel);

let catchAndLogError = WorkerUtils.catchAndLogError(logLabel);

let fiveSecondsInMilliseconds = 5000;

let syncInterval = fiveSecondsInMilliseconds;

let handleMsg = (venturesPromise, doWork, msg) =>
  Js.Promise.(
    venturesPromise
    |> then_(((storagePrefix, ventures)) =>
         switch (msg) {
         | SessionPending =>
           logMessage("Handling 'SessionPending'");
           (storagePrefix, VentureId.makeMap()) |> resolve;
         | SessionStarted(items, storagePrefix) =>
           logMessage("Handling 'SessionStarted'");
           items |> WorkerLocalStorage.setBlockstackItems;
           Venture.Index.load()
           |> then_(index =>
                index
                |. List.reduce(
                     resolve(VentureId.makeMap()),
                     (p, {id}: Venture.Index.item) =>
                     p
                     |> then_(ventures =>
                          WorkerUtils.loadVenture(id)
                          |> then_(venture => {
                               doWork(
                                 storagePrefix,
                                 [|(id, venture)|]
                                 |> Map.mergeMany(VentureId.makeMap()),
                               );
                               ventures |. Map.set(id, venture) |> resolve;
                             })
                          |> catch(err => {
                               logError(err);
                               resolve(ventures);
                             })
                        )
                   )
                |> then_(ventures => (storagePrefix, ventures) |> resolve)
              );
         | VentureLoaded(ventureId, log, _) =>
           logMessage("Handling 'VentureLoaded'");
           (storagePrefix, ventures |. Map.set(ventureId, log)) |> resolve;
         | VentureCreated(ventureId, log) =>
           logMessage("Handling 'VentureCreated'");
           (storagePrefix, ventures |. Map.set(ventureId, log)) |> resolve;
         | NewItems(ventureId, items) =>
           logMessage("Handling 'NewItems'");
           let venture = ventures |. Map.getExn(ventureId);
           (
             storagePrefix,
             ventures
             |. Map.set(ventureId, venture |> EventLog.appendItems(items)),
           )
           |> resolve;
         | NewIncomeAddress(_, _)
         | UpdateIndex(_) => (storagePrefix, ventures) |> resolve
         }
       )
  );

let intervalId: ref(option(Js.Global.intervalId)) = ref(None);

let venturesPromise: ref(Js.Promise.t((string, VentureId.map(EventLog.t)))) =
  ref(("", VentureId.makeMap()) |> Js.Promise.resolve);

onMessage(
  self,
  msg => {
    let doWork = (storagePrefix, ventures) => {
      IncomeCollection.collectIncome(ventures);
      LogSync.syncLogs(storagePrefix, ventures);
    };
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
                  |> then_(((storagePrefix, ventures)) =>
                       doWork(storagePrefix, ventures) |> resolve
                     )
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
