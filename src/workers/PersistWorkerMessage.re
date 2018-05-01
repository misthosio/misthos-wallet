open WorkerLocalStorage;

open PrimitiveTypes;

type send =
  | UpdateSession(userId, blockstackItems)
  | PersistVenture(ventureId);

type receive =
  | VenturePersisted(ventureId);

type encodedReceive = receive;

let decodeReceive = receive => receive;
