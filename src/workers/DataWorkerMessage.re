type incoming = VentureWorkerMessage.outgoing;

type outgoing = VentureWorkerMessage.incoming;

let encodeIncoming = VentureWorkerMessage.encodeOutgoing;

let decodeIncoming = VentureWorkerMessage.decodeOutgoing;

type encodedOutgoing = VentureWorkerMessage.encodedIncoming;

let decodeOutgoing = VentureWorkerMessage.decodeIncoming;
