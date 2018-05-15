[%bs.raw {| self.localStorage = require("./fakeLocalStorage").localStorage |}];

[%bs.raw
  {| self.window = { localStorage: self.localStorage , location: { origin: self.origin } } |}
];

module Message = SyncWorkerMessage;

type self;

[@bs.val] external self : self = "";

[@bs.set]
external onMessage :
  (self, [@bs.uncurry] ({. "data": Message.incoming} => unit)) => unit =
  "onmessage";

[@bs.val] external _postMessage : Js.Json.t => unit = "postMessage";

let postMessage = msg =>
  msg |> VentureWorkerMessage.encodeIncoming |> _postMessage;

open PrimitiveTypes;

let logMessage = msg => Js.log("[Sync Worker] - " ++ msg);

let intervalId: ref(option(Js.Global.intervalId)) = ref(None);

let fiveSecondsInMilliseconds = 5000;

let syncInterval = fiveSecondsInMilliseconds;

let determinPartnerIds =
  EventLog.reduce(
    (ids, {event}) =>
      switch (event) {
      | PartnerAccepted({data}) => [data.id, ...ids]
      | PartnerRemovalAccepted({data}) =>
        ids |> List.filter(id => UserId.neq(id, data.id))
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
           | items => postMessage(NewItemsDetected(ventureId, items))
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

let syncEventsFromVenture = (ventureId, storagePrefix) => {
  logMessage(
    "Finding new events for venture '" ++ VentureId.toString(ventureId) ++ "'",
  );
  Js.Promise.(
    WorkerUtils.loadVenture(ventureId)
    |> then_(eventLog => {
         let summary = eventLog |> EventLog.getSummary;
         let partnerKeys = eventLog |> determinPartnerIds;
         partnerKeys
         |> List.iter(
              syncEventsFromPartner(
                storagePrefix,
                ventureId,
                summary.knownItems,
                eventLog,
              ),
            );
         resolve();
       })
  );
};

let findNewEventsForAll = () =>
  Js.Promise.(
    Session.getCurrentSession()
    |> then_(
         fun
         | Session.LoggedIn({storagePrefix}) =>
           Venture.Index.load()
           |> then_(index =>
                index
                |> List.iter(({id}: Venture.Index.item) =>
                     syncEventsFromVenture(id, storagePrefix) |> ignore
                   )
                |> resolve
              )
         | _ => resolve(),
       )
  );

let handleMsg =
  fun
  | Message.UpdateSession(items) => {
      logMessage("Handling 'UpdateSession'");
      items |> WorkerLocalStorage.setBlockstackItems;
      findNewEventsForAll() |> ignore;
      Js.Global.setInterval(
        () => findNewEventsForAll() |> ignore,
        syncInterval,
      );
    };

onMessage(
  self,
  msg => {
    let newIntervalid = handleMsg(msg##data);
    intervalId^
    |> Utils.mapOption(id =>
         if (newIntervalid != id) {
           Js.Global.clearInterval(id);
         }
       )
    |> ignore;
    intervalId := Some(newIntervalid);
  },
);
