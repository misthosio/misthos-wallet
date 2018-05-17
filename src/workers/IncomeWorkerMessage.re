open WorkerLocalStorage;

type incoming =
  | UpdateSession(blockstackItems);

type outgoing = VentureWorkerMessage.incoming;

external encodeIncoming : incoming => Js.Json.t = "%identity";

external decodeIncoming : Js.Json.t => incoming = "%identity";

type encodedOutgoing = VentureWorkerMessage.encodedIncoming;

let decodeOutgoing = VentureWorkerMessage.decodeIncoming;
