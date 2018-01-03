module type Item = {
  type t;
  let getType: t => string;
  let encode: t => string;
  let decode: (string, ~type_: string) => t;
  type validatorState;
  let canKeyWitnessItem: (validatorState, t, string) => bool;
  let canKeySignItem: (validatorState, t, string) => bool;
  let validate: (validatorState, t) => (bool, validatorState);
};

type signature = (string, Bitcoin.ECSignature.t);

type item('t) = {
  payload: 't,
  type_: string,
  encodedPayload: string,
  hash: string,
  signature
};

type logEntry('t) = {
  item: 't,
  logHash: string,
  witnesses: list(signature)
};

exception Illegal;

module Make = (Payload: Item) => {
  type validatorState = Payload.validatorState;
  type unvalidated = list(logEntry(item(Payload.t)));
  type t = (validatorState, unvalidated);
  let make = (vState) => (vState, []);
  let createItem = (issuerKeyPair, payload) => {
    let encodedPayload = payload |> Payload.encode;
    let hashBuffer = encodedPayload |> Bitcoin.Crypto.sha256;
    let hash = hashBuffer |> Utils.toHex;
    let issuerPubKey = issuerKeyPair |> Utils.publicKeyFromKeyPair;
    let signature = (issuerPubKey, issuerKeyPair |> Bitcoin.ECPair.sign(hashBuffer));
    {payload, type_: Payload.getType(payload), encodedPayload, signature, hash}
  };
  let createEntry = (lastHash, keyPair, item) => {
    let logHashBuffer = lastHash ++ item.hash |> Bitcoin.Crypto.sha256;
    let logHash = logHashBuffer |> Utils.toHex;
    let signature = keyPair |> Bitcoin.ECPair.sign(logHashBuffer);
    {item, logHash, witnesses: [(keyPair |> Utils.publicKeyFromKeyPair, signature)]}
  };
  let append = (payload, keyPair, (vState, log)) => {
    let pubKey = Utils.publicKeyFromKeyPair(keyPair);
    let canSign = pubKey |> Payload.canKeySignItem(vState, payload);
    let canWitness = pubKey |> Payload.canKeyWitnessItem(vState, payload);
    let (isValid, newVState) = payload |> Payload.validate(vState);
    if (canSign && canWitness && isValid) {
      let newItem = payload |> createItem(keyPair);
      let newLog =
        switch log {
        | [] => [newItem |> createEntry("", keyPair)]
        | [e, ...rest] => [newItem |> createEntry(e.logHash, keyPair), e, ...rest]
        };
      (newVState, newLog)
    } else {
      raise(Illegal)
    }
  };
  let reduce = (reducer, start, (_vState, log)) =>
    log |> List.rev_map((entry) => entry.item.payload) |> List.fold_left(reducer, start);
  let merge = (log, otherLogs) => log;
  let validate = (validatorState, log) => (validatorState, log);
  let decode = (raw) => [];
  module Encode = {
    let ecSig = (ecSig) => Json.Encode.string(ecSig |> Bitcoin.ECSignature.toDER |> Utils.toHex);
    let signature = (signature) => Json.Encode.(pair(string, ecSig, signature));
    let item = (item) =>
      Json.Encode.(
        object_([
          ("type", string(item.type_)),
          ("payload", string(item.encodedPayload)),
          ("hash", string(item.hash)),
          ("sig", signature(item.signature))
        ])
      );
    let entry = (entry) =>
      Json.Encode.(
        object_([
          ("item", item(entry.item)),
          ("logHash", string(entry.logHash)),
          ("witnesses", list(signature, entry.witnesses))
        ])
      );
    let log = (log) => Json.Encode.(list(entry, log));
  };
  let encode = ((_vState, log)) => Encode.log(log) |> Json.stringify;
  /* module Decode = { */
  /*   let signature = (json) => */
  /*     Json.Decode.string(json) */
  /*     |> BufferExt.fromStringWithEncoding(~encoding="hex") */
  /*     |> Bitcoin.ECSignature.fromDER; */
  /*   let event = (raw) => { */
  /*     let json = Json.parseOrRaise(raw); */
  /*     let type_ = json |> Json.Decode.(field("type", string)); */
  /*     Json.Decode.{ */
  /*       type_, */
  /*       payload: Payload.decode(json |> field("payload", string), type_), */
  /*       hash: json |> field("hash", string), */
  /*       issuerPubKey: json |> field("issuerPubKey", string), */
  /*       signature: json |> field("signature", signature) */
  /*     } */
  /*   }; */
  /* }; */
};
