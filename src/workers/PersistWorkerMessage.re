open WorkerLocalStorage;

type incoming =
  | UpdateSession(blockstackItems)
  | VentureWorkerMessage(Js.Json.t);

type outgoing = unit;

type encodedIncoming = unit;

type encodedOutgoing = outgoing;

let decodeOutgoing = outgoing => outgoing;
