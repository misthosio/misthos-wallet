open WorkerLocalStorage;

open PrimitiveTypes;

type incoming =
  | UpdateSession(userId, blockstackItems)
  | VentureWorkerMessage(Js.Json.t);

type outgoing = unit;

type encodedOutgoing = outgoing;

let decodeOutgoing = outgoing => outgoing;
