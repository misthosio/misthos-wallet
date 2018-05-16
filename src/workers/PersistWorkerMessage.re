open WorkerLocalStorage;

type incoming =
  | UpdateSession(blockstackItems)
  | VentureWorkerMessage(Js.Json.t);

type outgoing = unit;

external encodeIncoming : incoming => Js.Json.t = "%identity";

external decodeOutgoing : Js.Json.t => outgoing = "%identity";
