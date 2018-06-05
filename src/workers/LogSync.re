[@bs.val] external _postMessage : WebWorker.message => unit = "postMessage";

let postMessage = msg =>
  {"payload": msg |> VentureWorkerMessage.encodeIncoming, "correlationId": ""}
  |> _postMessage;

open Belt;

open PrimitiveTypes;

let logLabel = "[Log Sync]";

let logMessage = WorkerUtils.logMessage(logLabel);

let catchAndLogError = WorkerUtils.catchAndLogError(logLabel);

let intervalId: ref(option(Js.Global.intervalId)) = ref(None);

let tenSecondsInMilliseconds = 10000;

let syncInterval = tenSecondsInMilliseconds;

let determinPartnerIds =
  EventLog.reduce(
    (ids, {event}) =>
      switch (event) {
      | PartnerAccepted({data}) => [data.id, ...ids]
      | PartnerRemovalAccepted({data}) =>
        ids |. List.keep(id => UserId.neq(id, data.id))
      | _ => ids
      },
    [],
  );

let getSummaryFromUser = (ventureId, userId, storagePrefix) =>
  Js.Promise.(
    Blockstack.getFileFromUserAndDecrypt(
      (ventureId |> VentureId.toString)
      ++ "/"
      ++ storagePrefix
      ++ "/summary.json",
      ~username=userId |> UserId.toString,
    )
    |> catch(_error => raise(Not_found))
    |> then_(nullFile =>
         switch (Js.Nullable.toOption(nullFile)) {
         | None => raise(Not_found)
         | Some(raw) =>
           raw |> Json.parseOrRaise |> EventLog.decodeSummary |> resolve
         }
       )
  );

let getLogFromUser = (ventureId, userId, storagePrefix) =>
  Js.Promise.(
    Blockstack.getFileFromUserAndDecrypt(
      (ventureId |> VentureId.toString) ++ "/" ++ storagePrefix ++ "/log.json",
      ~username=userId |> UserId.toString,
    )
    |> catch(_error => raise(Not_found))
    |> then_(nullFile =>
         switch (Js.Nullable.toOption(nullFile)) {
         | None => raise(Not_found)
         | Some(raw) => raw |> Json.parseOrRaise |> EventLog.decode |> resolve
         }
       )
  );

let findNewItemsFromPartner = (ventureId, userId, storagePrefix, eventLog) =>
  Js.Promise.(
    getLogFromUser(ventureId, userId, storagePrefix)
    |> then_(other =>
         (
           switch (eventLog |> EventLog.findNewItems(~other)) {
           | [||] => ()
           | items => postMessage(NewItemsDetected(ventureId, items, userId))
           }
         )
         |> resolve
       )
  )
  |> ignore;

let syncEventsFromPartner =
    (storagePrefix, ventureId, knownItems, eventLog, userId) =>
  Js.Promise.(
    getSummaryFromUser(ventureId, userId, storagePrefix)
    |> then_((otherSummary: EventLog.summary) =>
         (
           if (otherSummary.knownItems
               |. Belt.Set.String.subset(knownItems) == false) {
             findNewItemsFromPartner(
               ventureId,
               userId,
               storagePrefix,
               eventLog,
             );
           }
         )
         |> resolve
       )
    |> catch(_error => resolve())
  )
  |> ignore;

let syncEventsFromVenture = (storagePrefix, ventureId, eventLog) => {
  logMessage(
    "Finding new events for venture '" ++ VentureId.toString(ventureId) ++ "'",
  );
  let summary = eventLog |> EventLog.getSummary;
  eventLog
  |> determinPartnerIds
  |. List.forEach(
       syncEventsFromPartner(
         storagePrefix,
         ventureId,
         summary.knownItems,
         eventLog,
       ),
     );
};

let syncLogs = (storagePrefix, ventures: VentureId.map(EventLog.t)) =>
  ventures
  |. Map.forEachU((. id, log) =>
       log |> syncEventsFromVenture(storagePrefix, id)
     );
