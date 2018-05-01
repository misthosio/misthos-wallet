open WorkerLocalStorage;

open PrimitiveTypes;

type send =
  | InitializeLocalStorage(blockstackItems)
  | LoadVenture(ventureId)
  | VentureUpdated(ventureId);

type receive =
  | None;

let decodeReceive = (_) => None;
