module type Encodable = {
  type t;
  let encode: t => Js.Json.t;
  let decode: Js.Json.t => t;
};

type signature = (string, Bitcoin.ECSignature.t);

type entry('t) = {
  event: 't,
  hash: string,
  signature
};

type log('t) = list(entry('t));

module Make = (Event: Encodable) => {
  type t = log(Event.t);
  let make = () => [];
  let createEntry = (issuerKeyPair, event) => {
    let eventHash =
      event
      |> Event.encode
      |> Json.stringify
      |> Bitcoin.Crypto.sha256
      |> Utils.bufToHex;
    let issuerPubKey = issuerKeyPair |> Utils.publicKeyFromKeyPair;
    let hashBuffer = issuerPubKey ++ eventHash |> Bitcoin.Crypto.sha256;
    let hash = hashBuffer |> Utils.bufToHex;
    let signature = (
      issuerPubKey,
      issuerKeyPair |> Bitcoin.ECPair.sign(hashBuffer)
    );
    {event, signature, hash};
  };
  let append = (event, issuer, log) => {
    let item = event |> createEntry(issuer);
    [item, ...log];
  };
  let reduce = (reducer, start, log) =>
    log |> List.rev_map(entry => entry.event) |> List.fold_left(reducer, start);
  let decode = _raw => make();
  module Encode = {
    let ecSig = ecSig => Json.Encode.string(ecSig |> Utils.signatureToString);
    let signature = Json.Encode.(pair(string, ecSig));
    let entry = entry =>
      Json.Encode.(
        object_([
          ("event", Event.encode(entry.event)),
          ("hash", string(entry.hash)),
          ("sig", signature(entry.signature))
        ])
      );
    let log = Json.Encode.(list(entry));
  };
  let encode = Encode.log;
};
