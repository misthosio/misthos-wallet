[%bs.raw {| self.localStorage = require("./fakeLocalStorage").localStorage |}];

[%bs.raw
  {| self.window = { localStorage: self.localStorage , location: { origin: self.origin } } |}
];

module Message = PersistWorkerMessage;

type self;

[@bs.val] external self : self = "";

[@bs.set]
external onMessage :
  (self, [@bs.uncurry] ({. "data": Message.send} => unit)) => unit =
  "onmessage";

[@bs.val] external postMessage : Message.receive => unit = "postMessage";

open PrimitiveTypes;

let logMessage = msg => Js.log("[Persist Worker] - " ++ msg);

exception CouldNotLoadVenture;

let loadVenture = ventureId =>
  Js.Promise.(
    Blockstack.getFileDecrypted(
      (ventureId |> VentureId.toString) ++ "/log.json",
    )
    |> then_(nullLog =>
         switch (Js.Nullable.toOption(nullLog)) {
         | Some(raw) => raw |> Json.parseOrRaise |> EventLog.decode |> resolve
         | None => raise(CouldNotLoadVenture)
         }
       )
  );

let determinPartnerKeys =
  EventLog.reduce(
    (keys, {event}) =>
      switch (event) {
      | PartnerAccepted({data}) => [(data.id, data.pubKey), ...keys]
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
                  summaryString,
                )
              ),
         resolve(),
       )
    |> then_(() => postMessage(VenturePersisted(ventureId)) |> resolve)
  );
};

let handleMessage =
  fun
  | Message.InitializeLocalStorage(items) => {
      logMessage("Initializing localStorage");
      items |> WorkerLocalStorage.setBlockstackItems;
    }
  | PersistVenture(id) => {
      logMessage("Persisting venture '" ++ VentureId.toString(id) ++ "'");
      Js.Promise.(
        loadVenture(id)
        |> then_(eventLog =>
             eventLog |> determinPartnerKeys |> persist(id, eventLog)
           )
      )
      |> ignore;
    };

onMessage(self, msg => handleMessage(msg##data));
