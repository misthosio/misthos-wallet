[%bs.raw {| self.localStorage = require("./fakeLocalStorage").localStorage |}];

[%bs.raw
  {| self.window = { localStorage: self.localStorage , location: { origin: self.origin } } |}
];

open VentureWorkerMessage;

type self;

[@bs.val] external self : self = "";

[@bs.set]
external onMessage :
  (self, [@bs.uncurry] ({. "data": WebWorker.message} => unit)) => unit =
  "onmessage";

open PrimitiveTypes;

let logMessage = msg => Js.log("[Persist Worker] - " ++ msg);

let determinPartnerKeysAndRemovals = eventLog => {
  let (partners, keys, _, removalProcesses) =
    eventLog
    |> EventLog.reduce(
         (
           (partners, keys, processLookup, removalProcesses),
           {event} as item,
         ) =>
           switch (event) {
           | PartnerAccepted({data}) => (
               [data.id, ...partners],
               [(data.id, data.pubKey), ...keys],
               processLookup,
               removalProcesses,
             )
           | PartnerRemovalProposed({processId, data: {id}}) =>
             let removals =
               try (removalProcesses |> List.assoc(id)) {
               | Not_found => []
               };
             (
               partners,
               keys,
               [(processId, id), ...processLookup],
               [
                 (id, [item, ...removals]),
                 ...removalProcesses |> List.remove_assoc(id),
               ],
             );
           | PartnerRemovalEndorsed({processId}) =>
             let id = processLookup |> List.assoc(processId);
             let removals = removalProcesses |> List.assoc(id);
             (
               partners,
               keys,
               processLookup,
               [
                 (id, [item, ...removals]),
                 ...removalProcesses |> List.remove_assoc(id),
               ],
             );
           | PartnerRemovalRejected({processId}) =>
             let id = processLookup |> List.assoc(processId);
             let removals = removalProcesses |> List.assoc(id);
             (
               partners,
               keys,
               processLookup,
               [
                 (id, [item, ...removals]),
                 ...removalProcesses |> List.remove_assoc(id),
               ],
             );
           | PartnerRemovalAccepted({data: {id}}) =>
             let removals = removalProcesses |> List.assoc(id);
             (
               partners |> List.filter(UserId.neq(id)),
               keys,
               processLookup,
               [
                 (id, [item, ...removals]),
                 ...removalProcesses |> List.remove_assoc(id),
               ],
             );
           | CustodianRemovalProposed({processId, data: {custodianId}}) =>
             let removals =
               try (removalProcesses |> List.assoc(custodianId)) {
               | Not_found => []
               };
             (
               partners,
               keys,
               [(processId, custodianId), ...processLookup],
               [
                 (custodianId, [item, ...removals]),
                 ...removalProcesses |> List.remove_assoc(custodianId),
               ],
             );
           | CustodianRemovalEndorsed({processId}) =>
             let id = processLookup |> List.assoc(processId);
             let removals = removalProcesses |> List.assoc(id);
             (
               partners,
               keys,
               processLookup,
               [
                 (id, [item, ...removals]),
                 ...removalProcesses |> List.remove_assoc(id),
               ],
             );
           | CustodianRemovalRejected({processId}) =>
             let id = processLookup |> List.assoc(processId);
             let removals = removalProcesses |> List.assoc(id);
             (
               partners,
               keys,
               processLookup,
               [
                 (id, [item, ...removals]),
                 ...removalProcesses |> List.remove_assoc(id),
               ],
             );
           | CustodianRemovalAccepted({data: {custodianId}}) =>
             let removals = removalProcesses |> List.assoc(custodianId);
             (
               partners |> List.filter(UserId.neq(custodianId)),
               keys,
               processLookup,
               [
                 (custodianId, [item, ...removals]),
                 ...removalProcesses |> List.remove_assoc(custodianId),
               ],
             );
           | _ => (partners, keys, processLookup, removalProcesses)
           },
         ([], [], [], []),
       );
  (
    keys |> List.filter(((id, _)) => partners |> List.mem(id)),
    (
      removalProcesses
      |> List.filter(((id, _)) => partners |> List.mem(id) == false),
      keys,
    ),
  );
};

let persistLogString = (ventureId, logString, pubKey) =>
  Blockstack.putFileNotEncrypted(
    (ventureId |> VentureId.toString)
    ++ "/"
    ++ UserInfo.storagePrefix(~appPubKey=pubKey)
    ++ "/log.json",
    logString |> Blockstack.encryptECIES(~publicKey=pubKey) |> Json.stringify,
  );

let persistSummaryString = (ventureId, summaryString, pubKey) =>
  Blockstack.putFileNotEncrypted(
    (ventureId |> VentureId.toString)
    ++ "/"
    ++ UserInfo.storagePrefix(~appPubKey=pubKey)
    ++ "/summary.json",
    summaryString
    |> Blockstack.encryptECIES(~publicKey=pubKey)
    |> Json.stringify,
  );

let persistRemovals = (ventureId, (removalProcesses, removedKeys)) =>
  Js.Promise.(
    removalProcesses
    |> List.fold_left(
         (promise, (id, items)) => {
           let pubKey = removedKeys |> List.assoc(id);
           switch (pubKey) {
           | Some(pubKey) =>
             let eventLog =
               items
               |> List.rev
               |> List.fold_left(
                    (log, item) => log |> EventLog.appendItem(item),
                    EventLog.make(),
                  );
             promise
             |> then_(() =>
                  persistLogString(
                    ventureId,
                    eventLog |> EventLog.encode |> Json.stringify,
                    pubKey,
                  )
                )
             |> then_(() =>
                  persistSummaryString(
                    ventureId,
                    eventLog
                    |> EventLog.getSummary
                    |> EventLog.encodeSummary
                    |> Json.stringify,
                    pubKey,
                  )
                );
           | None => promise
           };
         },
         resolve(),
       )
  );

let missingKeys = ref(Belt.Map.String.empty);
let addToMissingKeys = (ventureId, userId, f) => {
  open Belt;
  let key = VentureId.toString(ventureId) ++ UserId.toString(userId);
  missingKeys := missingKeys^ |. Map.String.set(key, f);
};
let removeVentureFromMissingKeys = ventureId => {
  open Belt;
  let ventureStr = VentureId.toString(ventureId);
  missingKeys :=
    missingKeys^
    |. Map.String.keepU((. key, _) =>
         Js.String.startsWith(ventureStr, key) == false
       );
};

Js.Global.setInterval(
  () =>
    Belt.(
      Js.Promise.(
        missingKeys^
        |. Map.String.reduceU(resolve(), (. promise, key, (id, f)) =>
             UserInfo.Public.(
               read(~blockstackId=id)
               |> then_(
                    fun
                    | UserInfo.Public.Ok({appPubKey}) =>
                      if (missingKeys^ |. Map.String.has(key)) {
                        logMessage("Missing key has been found");
                        promise
                        |> f(appPubKey)
                        |> then_(() => {
                             missingKeys :=
                               missingKeys^ |. Map.String.remove(key);
                             resolve();
                           });
                      } else {
                        promise;
                      }
                    | NotFound => {
                        logMessage("Could not find UserInfo");
                        promise;
                      },
                  )
             )
           )
        |> ignore
      )
    ),
  10000,
);

let persist = (ventureId, eventLog, (keys, removals)) => {
  let logString = eventLog |> EventLog.encode |> Json.stringify;
  let summaryString =
    eventLog |> EventLog.getSummary |> EventLog.encodeSummary |> Json.stringify;
  let persistLogAndSummary = (pubKey, promise) =>
    Js.Promise.(
      promise
      |> then_(() => persistLogString(ventureId, logString, pubKey))
      |> then_(() => persistSummaryString(ventureId, summaryString, pubKey))
    );
  Js.Promise.(
    keys
    |> List.fold_left(
         (promise, (id, pubKey)) => {
           removeVentureFromMissingKeys(ventureId);
           switch (pubKey) {
           | Some(pubKey) => promise |> persistLogAndSummary(pubKey)
           | None =>
             UserInfo.Public.(
               read(~blockstackId=id)
               |> then_(
                    fun
                    | UserInfo.Public.Ok({appPubKey}) =>
                      promise |> persistLogAndSummary(appPubKey)
                    | NotFound => {
                        addToMissingKeys(
                          ventureId,
                          id,
                          (id, persistLogAndSummary),
                        );
                        promise;
                      },
                  )
             )
           };
         },
         resolve(),
       )
    |> then_(() => resolve(removals))
  );
};

let persistVenture = ventureId => {
  logMessage("Persisting venture '" ++ VentureId.toString(ventureId) ++ "'");
  Js.Promise.(
    Session.getCurrentSession()
    |> then_(
         fun
         | Session.LoggedIn(_) =>
           WorkerUtils.loadVenture(ventureId)
           |> then_(eventLog =>
                eventLog
                |> determinPartnerKeysAndRemovals
                |> persist(ventureId, eventLog)
              )
           |> then_(persistRemovals(ventureId))
         | _ => resolve(),
       )
  );
};

let handleMessage =
  fun
  | SessionStarted(items, _storagePrefix) => {
      logMessage("Handling 'SessionStarted'");
      items |> WorkerLocalStorage.setBlockstackItems;
      Js.Promise.(
        Venture.Index.load()
        |> then_(({ventures}: Venture.Index.t) =>
             ventures
             |. Belt.List.reduce(resolve(), (p, {id}: Venture.Index.item) =>
                  p |> then_(() => persistVenture(id))
                )
           )
      );
    }
  | VentureLoaded(ventureId, _, newItems) when newItems |> Array.length > 0 =>
    persistVenture(ventureId)
  | VentureCreated(ventureId, _) => persistVenture(ventureId)
  | NewItems(ventureId, _) => persistVenture(ventureId)
  | _ => Js.Promise.resolve();

onMessage(self, msg =>
  handleMessage(msg##data##payload |> PersistWorkerMessage.decodeIncoming)
  |> ignore
);
