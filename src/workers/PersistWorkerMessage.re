open WorkerLocalStorage;

open PrimitiveTypes;

type send =
  | InitializeLocalStorage(userId, blockstackItems)
  | PersistVenture(ventureId);

type receive =
  | VenturePersisted(ventureId);

type encodedReceive = receive;

let decodeReceive = receive => receive;
