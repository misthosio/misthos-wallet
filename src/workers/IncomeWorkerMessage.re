open WorkerLocalStorage;

type exposedAddresses = list(string);

type txIds = list(string);

type incoming =
  | UpdateSession(blockstackItems);

type outgoing = VentureWorkerMessage.incoming;

type encodedIncoming = unit;

type encodedOutgoing = VentureWorkerMessage.encodedIncoming;

let decodeOutgoing = VentureWorkerMessage.decodeIncoming;
