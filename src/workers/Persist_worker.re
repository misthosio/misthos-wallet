[%bs.raw {| self.localStorage = require("./fakeLocalStorage").localStorage |}];

[%bs.raw {| self.window = { localStorage: self.localStorage } |}];

module Message = PersistWorkerMessage;

type self;

[@bs.val] external self : self = "";

[@bs.set]
external onMessage :
  (self, [@bs.uncurry] ({. "data": Message.send} => unit)) => unit =
  "onmessage";

open PrimitiveTypes;

let logMessage = msg => Js.log("[Persist Worker] - " ++ msg);

let handleMessage =
  fun
  | Message.InitializeLocalStorage(items) => {
      logMessage("Initializing local storage");
      items |> WorkerLocalStorage.setBlockstackItems;
    }
  | LoadVenture(id) =>
    logMessage("Loading venture '" ++ VentureId.toString(id) ++ "'")
  | VentureUpdated(id) =>
    logMessage("Venture '" ++ VentureId.toString(id) ++ "' was updated");

onMessage(self, msg => handleMessage(msg##data));
