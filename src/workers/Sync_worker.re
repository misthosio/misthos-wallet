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

let determinPartnerKeys = localUserId =>
  EventLog.reduce(
    (keys, {event}) =>
      switch (event) {
      | PartnerAccepted({data}) when UserId.neq(data.id, localUserId) => [
          (data.id, data.pubKey),
          ...keys,
        ]
      | PartnerRemovalAccepted({data}) => keys |> List.remove_assoc(data.id)
      | _ => keys
      },
    [],
  );

let getSummaryFromUser = (ventureId, userId, pubKey) =>
  Js.Promise.(
    Blockstack.getFileFromUserAndDecrypt(
      (ventureId |> VentureId.toString)
      ++ "/"
      ++ UserInfo.storagePrefix(~appPubKey=pubKey)
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

let syncEventsFromPartner = (ventureId, summary, (userId, pubKey)) =>
  Js.Promise.(
    getSummaryFromUser(ventureId, userId, pubKey)
    |> then_(summary => Js.log2("got summary", summary) |> resolve)
  )
  |> ignore;

let syncEventsFromVenture = (ventureId, localUserId) => {
  logMessage(
    "Finding new events for venture '" ++ VentureId.toString(ventureId) ++ "'",
  );
  Js.Promise.(
    WorkerUtils.loadVenture(ventureId)
    |> then_(eventLog => {
         let summary = eventLog |> EventLog.getSummary;
         let partnerKeys = eventLog |> determinPartnerKeys(localUserId);
         partnerKeys |> List.iter(syncEventsFromPartner(ventureId, summary));
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
         | Session.LoggedIn({userId}) =>
           Venture.Index.load()
           |> then_(index =>
                index
                |> List.iter(({id}: Venture.Index.item) =>
                     syncEventsFromVenture(id, userId) |> ignore
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
