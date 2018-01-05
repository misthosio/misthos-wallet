module type Encodable = {
  type t;
  let hash: t => string;
  let encode: t => Js.Json.t;
  let decode: Js.Json.t => t;
};

type itemSig = (string, Bitcoin.ECSignature.t);

type item('t) = {
  event: 't,
  hash: string,
  signature: itemSig
};

type logEntry('t) = {
  item: item('t),
  clock: VectorClock.t
};

module Make = (Event: Encodable) => {
  type t = (string, list(logEntry(Event.t)));
  let make = id => (id, []);
  let createItem = (issuerKeyPair, event) => {
    let encodedEvent = event |> Event.hash;
    let issuerPubKey = issuerKeyPair |> Utils.publicKeyFromKeyPair;
    let hashBuffer = issuerPubKey ++ encodedEvent |> Bitcoin.Crypto.sha256;
    let hash = hashBuffer |> Utils.bufToHex;
    let signature = (
      issuerPubKey,
      issuerKeyPair |> Bitcoin.ECPair.sign(hashBuffer)
    );
    {event, signature, hash};
  };
  let append = (event, issuer, (id, entries)) => {
    let item = event |> createItem(issuer);
    let clock =
      switch entries {
      | [] => VectorClock.make()
      | [e, ..._rest] => e.clock |> VectorClock.increment(id)
      };
    (id, [{clock, item}, ...entries]);
  };
  let reduce = (reducer, start, (_, entries)) =>
    entries
    |> List.rev_map(entry => entry.item.event)
    |> List.fold_left(reducer, start);
  let decode = _raw => ("", []);
  module Encode = {
    let ecSig = ecSig => Json.Encode.string(ecSig |> Utils.signatureToString);
    let itemSig = signature => Json.Encode.(pair(string, ecSig, signature));
    let item = item =>
      Json.Encode.(
        object_([
          ("event", Event.encode(item.event)),
          ("hash", string(item.hash)),
          ("sig", itemSig(item.signature))
        ])
      );
    let entry = entry =>
      Json.Encode.(
        object_([
          ("item", item(entry.item)),
          ("clock", VectorClock.encode(entry.clock))
        ])
      );
    let log = ((id, entries)) =>
      Json.Encode.(
        object_([("clockId", string(id)), ("entries", list(entry, entries))])
      );
  };
  let encode = log => Encode.log(log);
};
