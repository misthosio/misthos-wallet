open WorkerLocalStorage;

open PrimitiveTypes;

type send =
  | UpdateSession(userId, blockstackItems)
  | VentureWorkerMessage(Js.Json.t);

type receive = unit;

type encodedReceive = receive;

let decodeReceive = receive => receive;
