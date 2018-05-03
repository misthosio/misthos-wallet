open WorkerLocalStorage;

type exposedAddresses = list(string);

type txIds = list(string);

type incoming =
  | UpdateSession(blockstackItems);

type outgoing = VentureWorkerMessage.incoming;

type encodedOutgoing = outgoing;

let decodeOutgoing = a => a;
