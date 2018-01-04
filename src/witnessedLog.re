module type Encodable = {
  type t;
  let encode: t => string;
  let decode: string => t;
};

type pubKey = string;

type itemSig = (pubKey, Bitcoin.ECSignature.t);

type witnessSig = (pubKey, (string, Bitcoin.ECSignature.t));

type item('t) = {
  event: 't,
  encodedEvent: string,
  hash: string,
  signature: itemSig
};

type logEntry('t) = {
  item: item('t),
  logHash: string
};

type log('t) = {
  entries: list(logEntry('t)),
  witnesses: list(witnessSig)
};

module Make = (Event: Encodable) => {
  type t = log(Event.t);
  let make = () => {witnesses: [], entries: []};
  let createItem = (issuerKeyPair, event) => {
    let encodedEvent = event |> Event.encode;
    let issuerPubKey = issuerKeyPair |> Utils.publicKeyFromKeyPair;
    let hashBuffer = issuerPubKey ++ encodedEvent |> Bitcoin.Crypto.sha256;
    let hash = hashBuffer |> Utils.toHex;
    let signature = (
      issuerPubKey,
      issuerKeyPair |> Bitcoin.ECPair.sign(hashBuffer)
    );
    {event, encodedEvent, signature, hash};
  };
  let createEntry = (lastHash, witness, item) => {
    let (_, itemSig) = item.signature;
    let logHashBuffer =
      lastHash ++ (itemSig |> Utils.signatureToString) |> Bitcoin.Crypto.sha256;
    let logHash = logHashBuffer |> Utils.toHex;
    let signature = witness |> Bitcoin.ECPair.sign(logHashBuffer);
    ({item, logHash}, (logHash, signature));
  };
  let append = (event, issuer, {entries, witnesses}) => {
    let witness = issuer;
    let lastLogHash =
      switch entries {
      | [] => ""
      | [e, ..._rest] => e.logHash
      };
    let witnessPubKey = witness |> Utils.publicKeyFromKeyPair;
    let (newEntry, signature) =
      event |> createItem(issuer) |> createEntry(lastLogHash, witness);
    {
      entries: [newEntry, ...entries],
      witnesses: [
        (witnessPubKey, signature),
        ...witnesses |> List.remove_assoc(witnessPubKey)
      ]
    };
  };
  let reduce = (reducer, start, {entries}) =>
    entries
    |> List.rev_map(entry => entry.item.event)
    |> List.fold_left(reducer, start);
  let decode = raw => {entries: [], witnesses: []};
  module Encode = {
    let ecSig = ecSig => Json.Encode.string(ecSig |> Utils.signatureToString);
    let itemSig = signature => Json.Encode.(pair(string, ecSig, signature));
    let item = item =>
      Json.Encode.(
        object_([
          ("event", string(item.encodedEvent)),
          ("hash", string(item.hash)),
          ("sig", itemSig(item.signature))
        ])
      );
    let entry = entry =>
      Json.Encode.(
        object_([
          ("item", item(entry.item)),
          ("logHash", string(entry.logHash))
        ])
      );
    let witnessSig = ((pubKey, (hash, signature))) =>
      Json.Encode.(tuple3(string, string, ecSig, (pubKey, hash, signature)));
    let log = ({entries, witnesses}) =>
      Json.Encode.(
        object_([
          ("witnesses", list(witnessSig, witnesses)),
          ("entries", list(entry, entries))
        ])
      );
  };
  let encode = log => Encode.log(log) |> Json.stringify;
  /* module Decode = { */
  /*   let signature = (json) => */
  /*     Json.Decode.string(json) */
  /*     |> BufferExt.fromStringWithEncoding(~encoding="hex") */
  /*     |> Bitcoin.ECSignature.fromDER; */
  /*   let payload = (raw) => { */
  /*     let json = Json.parseOrRaise(raw); */
  /*     let type_ = json |> Json.Decode.(field("type", string)); */
  /*     Json.Decode.{ */
  /*       type_, */
  /*       payload: payload.decode(json |> field("payload", string), type_), */
  /*       hash: json |> field("hash", string), */
  /*       issuerPubKey: json |> field("issuerPubKey", string), */
  /*       signature: json |> field("signature", signature) */
  /*     } */
  /*   }; */
  /* }; */
  /* For Testing: */
  let head = ({entries}) =>
    switch entries {
    | [h, ..._rest] => h.logHash
    | [] => ""
    };
  module Merge = {
    let signEntry = (witness, {logHash}) => {
      let logHashBuffer = logHash |> Utils.fromHex;
      let signature = witness |> Bitcoin.ECPair.sign(logHashBuffer);
      (logHash, signature);
    };
    let updateWitnesses = (witness, whiteList, {witnesses, entries}) => {
      let witnessPubKey = Utils.publicKeyFromKeyPair(witness);
      {
        witnesses: [
          (witnessPubKey, signEntry(witness, List.hd(entries))),
          ...witnesses
             |> List.remove_assoc(witnessPubKey)
             |> List.filter(((pubKey, _)) => whiteList |> List.mem(pubKey))
        ],
        entries
      };
    };
    type scoreCounter = {
      found: int,
      score: int,
      length: int,
      search: list(witnessSig)
    };
    let witnessScore = ({witnesses, entries}) => {
      let state = {found: 0, score: 0, length: 0, search: witnesses};
      let score =
        entries
        |> List.fold_left(
             ({found, score, length, search}, {logHash}) =>
               switch search {
               | [] => {
                   found,
                   score: score + found,
                   length: length + 1,
                   search
                 }
               | _ =>
                 let (hits, rest) =
                   witnesses
                   |> List.partition(((_, (hash, _))) => hash == logHash);
                 let nHits = List.length(hits);
                 {
                   found: found + nHits,
                   score: score + found + nHits,
                   length: length + 1,
                   search: rest
                 };
               },
             state
           );
      (score.score, score.length);
    };
    let exec = (witness, whiteList, otherLogs, log) => {
      let updatedLogs = [
        log,
        ...otherLogs |> List.map(updateWitnesses(witness, whiteList))
      ];
      let scores = updatedLogs |> List.map(witnessScore);
      let scoredLogs =
        List.combine(scores, updatedLogs)
        |> List.sort_uniq(
             (((scoreA, lengthA), logA), ((scoreB, lengthB), logB)) =>
             if (head(logB) == head(logA)) {
               0;
             } else {
               switch (compare(scoreB, scoreA)) {
               | 0 =>
                 switch (compare(lengthB, lengthA)) {
                 | 0 => compare(head(logB), head(logA))
                 | n => n
                 }
               | n => n
               };
             }
           );
      let [(_, best), ..._rest] = scoredLogs;
      best;
    };
  };
  let merge = Merge.exec;
  /* For Testing: */
  let hasWitnessed = (pubKey, {witnesses}) =>
    witnesses |> List.mem_assoc(pubKey);
};
