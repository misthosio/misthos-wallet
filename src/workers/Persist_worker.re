[%bs.raw {| self.localStorage = require("./fakeLocalStorage").localStorage |}];

[%bs.raw
  {| self.window = { localStorage: self.localStorage , location: { origin: self.origin } } |}
];

module Message = PersistWorkerMessage;

type self;

[@bs.val] external self : self = "";

[@bs.set]
external onMessage :
  (self, [@bs.uncurry] ({. "data": Message.incoming} => unit)) => unit =
  "onmessage";

[@bs.val] external postMessage : Message.outgoing => unit = "postMessage";

open PrimitiveTypes;

let logMessage = msg => Js.log("[Persist Worker] - " ++ msg);

let determinPartnerKeysAndRemovals = (localUserId, eventLog) => {
  let (partners, keys, _, removalProcesses) =
    eventLog
    |> EventLog.reduce(
         (
           (partners, keys, processLookup, removalProcesses),
           {event} as item,
         ) =>
           switch (event) {
           | PartnerAccepted({data}) when UserId.neq(data.id, localUserId) => (
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
         },
         resolve(),
       )
  );

let persist = (ventureId, eventLog, (keys, removals)) => {
  let logString = eventLog |> EventLog.encode |> Json.stringify;
  let summaryString =
    eventLog |> EventLog.getSummary |> EventLog.encodeSummary |> Json.stringify;
  Js.Promise.(
    keys
    |> List.fold_left(
         (promise, (_id, pubKey)) =>
           promise
           |> then_(() => persistLogString(ventureId, logString, pubKey))
           |> then_(() =>
                persistSummaryString(ventureId, summaryString, pubKey)
              ),
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
         | Session.LoggedIn({userId}) =>
           WorkerUtils.loadVenture(ventureId)
           |> then_(eventLog =>
                eventLog
                |> determinPartnerKeysAndRemovals(userId)
                |> persist(ventureId, eventLog)
              )
           |> then_(persistRemovals(ventureId))
         | _ => resolve(),
       )
  )
  |> ignore;
};

let handleMessage =
  fun
  | Message.UpdateSession(items) => {
      logMessage("Handling 'UpdateSession'");
      items |> WorkerLocalStorage.setBlockstackItems;
    }
  | VentureWorkerMessage(raw) =>
    switch (raw |> VentureWorkerMessage.decodeOutgoing) {
    | VentureLoaded(ventureId, _) => persistVenture(ventureId)
    | VentureCreated(ventureId, _) => persistVenture(ventureId)
    | NewEvents(ventureId, _) => persistVenture(ventureId)
    | UpdateIndex(_) => ()
    };

onMessage(self, msg => handleMessage(msg##data));
