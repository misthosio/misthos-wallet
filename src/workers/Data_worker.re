[%bs.raw {| self.localStorage = require("./fakeLocalStorage").localStorage |}];

[%bs.raw
  {| self.window = { localStorage: self.localStorage , location: { origin: self.origin } } |}
];

module Message = IncomeWorkerMessage;

type self;

[@bs.val] external self : self = "";

[@bs.set]
external onMessage :
  (self, [@bs.uncurry] ({. "data": WebWorker.payload} => unit)) => unit =
  "onmessage";

[@bs.val] external _postMessage : WebWorker.payload => unit = "postMessage";

let postMessage = msg =>
  {
    "msg": msg |> VentureWorkerMessage.encodeIncoming,
    "syncId": WebWorker.emptySyncId,
  }
  |> _postMessage;

let logMessage = msg => Js.log("[Income Worker] - " ++ msg);

let tenSecondsInMilliseconds = 10000;

let syncInterval = tenSecondsInMilliseconds;

let handleMsg =
  fun
  | Message.UpdateSession(items) => {
      logMessage("Handling 'UpdateSession'");
      items |> WorkerLocalStorage.setBlockstackItems;
      Js.Global.setInterval(() => (), syncInterval);
    };

let intervalId: ref(option(Js.Global.intervalId)) = ref(None);

onMessage(
  self,
  msg => {
    let newIntervalid =
      handleMsg(msg##data##msg |> IncomeWorkerMessage.decodeIncoming);
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
