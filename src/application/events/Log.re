module type Encodable = {
  type t;
  let encode: t => Js.Json.t;
  let decode: Js.Json.t => t;
};

module Make = (Event: Encodable) => {
  type item = {
    event: Event.t,
    hash: string,
    issuerPubKey: string,
    signature: Node.buffer,
  };
  type t = array(item);
  type summary = {knownItems: Belt.Set.String.t};
  let make = () => [||];
  let makeItemHash = (issuerPubKey, event) => {
    let issuerPubKeyHash = issuerPubKey |> Bitcoin.Crypto.sha256;
    let eventHash =
      event
      |> Event.encode
      |> HashHelper.pruneNullFields
      |> JsonStable.stringify
      |> Bitcoin.Crypto.sha256;
    [|issuerPubKeyHash, eventHash|]
    |> BufferExt.concat
    |> Bitcoin.Crypto.sha256FromBuffer;
  };
  let items = log => log;
  let makeItem = (issuerKeyPair, event) => {
    let issuerPubKey = Utils.publicKeyFromKeyPair(issuerKeyPair);
    let hashBuffer = makeItemHash(issuerPubKey, event);
    {
      event,
      hash: hashBuffer |> Utils.bufToHex,
      issuerPubKey,
      signature: issuerKeyPair |> Bitcoin.ECPair.sign(hashBuffer),
    };
  };
  let append = (issuer, event, log) => {
    let item = event |> makeItem(issuer);
    (item, Array.append(log, [|item|]));
  };
  let appendItem = (item, log) => Array.append(log, [|item|]);
  let appendItems = (items, log) => Array.append(log, items);
  let reduce = (reducer, start, log) =>
    log |> Array.fold_left(reducer, start);
  let findNewItems = (~other, log) => {
    let existingHashes =
      log |> Array.map(({hash}) => hash) |> Belt.Set.String.fromArray;
    other
    |> Array.fold_left(
         (found, {hash} as item) =>
           if (found
               |> Js.Array.find(((foundHash, _)) => foundHash == hash)
               |> Js.Option.isSome) {
             found;
           } else if (existingHashes |. Belt.Set.String.has(hash)) {
             found;
           } else {
             Array.append(found, [|(hash, item)|]);
           },
         [||],
       )
    |. Belt.Array.keepMapU(
         (. (_, {issuerPubKey, event, hash, signature} as item)) => {
         let hashCheck = makeItemHash(issuerPubKey, event);
         if (hashCheck |> Utils.bufToHex != hash) {
           None;
         } else if (Utils.keyFromPublicKey(issuerPubKey)
                    |> Bitcoin.ECPair.verify(hashCheck, signature)) {
           Some(item);
         } else {
           None;
         };
       });
  };
  let length = Array.length;
  let getSummary = log => {
    knownItems:
      log |> Array.map(({hash}) => hash) |> Belt.Set.String.fromArray,
  };
  let encodeSummary = summary =>
    Json.Encode.(
      object_([
        (
          "knownItems",
          array(string, summary.knownItems |> Belt.Set.String.toArray),
        ),
      ])
    );
  let decodeSummary = raw =>
    Json.Decode.{
      knownItems:
        raw
        |> field("knownItems", array(string))
        |> Belt.Set.String.fromArray,
    };
  module Encode = {
    let ecSig = ecSig => Json.Encode.string(ecSig |> Utils.signatureToDER);
    let item = item =>
      Json.Encode.(
        object_([
          ("event", Event.encode(item.event)),
          ("issuerPubKey", string(item.issuerPubKey)),
          ("hash", string(item.hash)),
          ("signature", ecSig(item.signature)),
        ])
      );
    let log = log => Json.Encode.(object_([("items", array(item, log))]));
  };
  let encodeItem = Encode.item;
  let encode = Encode.log;
  module Decode = {
    let ecSig = ecSig => ecSig |> Json.Decode.string |> Utils.signatureFromDER;
    let item = item =>
      Json.Decode.{
        event: item |> field("event", Event.decode),
        hash: item |> field("hash", string),
        issuerPubKey: item |> field("issuerPubKey", string),
        signature: item |> field("signature", ecSig),
      };
    let log = Json.Decode.(field("items", array(item)));
  };
  let decodeItem = Decode.item;
  let decode = Decode.log;
};
