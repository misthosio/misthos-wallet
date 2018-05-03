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

let persist = (ventureId, eventLog, keys) => {
  let logString = eventLog |> EventLog.encode |> Json.stringify;
  let summaryString =
    eventLog |> EventLog.getSummary |> EventLog.encodeSummary |> Json.stringify;
  Js.Promise.(
    keys
    |> List.fold_left(
         (promise, (_id, pubKey)) =>
           promise
           |> then_(() =>
                Blockstack.putFileNotEncrypted(
                  (ventureId |> VentureId.toString)
                  ++ "/"
                  ++ UserInfo.storagePrefix(~appPubKey=pubKey)
                  ++ "/log.json",
                  logString
                  |> Blockstack.encryptECIES(~publicKey=pubKey)
                  |> Json.stringify,
                )
              )
           |> then_(() =>
                Blockstack.putFileNotEncrypted(
                  (ventureId |> VentureId.toString)
                  ++ "/"
                  ++ UserInfo.storagePrefix(~appPubKey=pubKey)
                  ++ "/summary.json",
                  summaryString
                  |> Blockstack.encryptECIES(~publicKey=pubKey)
                  |> Json.stringify,
                )
              ),
         resolve(),
       )
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
                |> determinPartnerKeys(userId)
                |> persist(ventureId, eventLog)
              )
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
