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

[@bs.val] external postMessage : Message.outgoing => unit = "postMessage";

open PrimitiveTypes;

let logMessage = msg => Js.log("[Sync Worker] - " ++ msg);

let intervalId: ref(option(Js.Global.intervalId)) = ref(None);

let tenSecondsInMilliseconds = 10000;

let syncInterval = tenSecondsInMilliseconds;

let determinPartnerIds = localUserId =>
  EventLog.reduce(
    (ids, {event}) =>
      switch (event) {
      | PartnerAccepted({data}) when UserId.neq(data.id, localUserId) => [
          data.id,
          ...ids,
        ]
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

let syncEventsFromPartner = (storagePrefix, ventureId, summary, userId) =>
  Js.Promise.(
    getSummaryFromUser(ventureId, userId, storagePrefix)
    |> then_(summary => Js.log2("got summary", summary) |> resolve)
    |> catch(_error => resolve())
  )
  |> ignore;

let syncEventsFromVenture = (ventureId, localUserId, storagePrefix) => {
  logMessage(
    "Finding new events for venture '" ++ VentureId.toString(ventureId) ++ "'",
  );
  Js.Promise.(
    WorkerUtils.loadVenture(ventureId)
    |> then_(eventLog => {
         let summary = eventLog |> EventLog.getSummary;
         let partnerKeys = eventLog |> determinPartnerIds(localUserId);
         partnerKeys
         |> List.iter(
              syncEventsFromPartner(storagePrefix, ventureId, summary),
            );
         resolve();
       })
  );
};

let findNewEventsForAll = () => {
  logMessage("Finding new events");
  Js.Promise.(
    Session.getCurrentSession()
    |> then_(
         fun
         | Session.LoggedIn({userId, storagePrefix}) =>
           Venture.Index.load()
           |> then_(index =>
                index
                |> List.iter(({id}: Venture.Index.item) =>
                     syncEventsFromVenture(id, userId, storagePrefix)
                     |> ignore
                   )
                |> resolve
              )
         | _ => resolve(),
       )
  );
};

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
