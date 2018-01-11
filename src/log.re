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
    signature: Bitcoin.ECSignature.t
  };
  type t = list(item);
  let make = () => [];
  let makeItemHash = (issuerPubKey, event) => {
    let issuerPubKeyHash = issuerPubKey |> Bitcoin.Crypto.sha256;
    let eventHash =
      event |> Event.encode |> Json.stringify |> Bitcoin.Crypto.sha256;
    [|issuerPubKeyHash, eventHash|]
    |> BufferExt.concat
    |> BufferExt.toString
    |> Bitcoin.Crypto.sha256;
  };
  let makeItem = (issuerKeyPair, event) => {
    let issuerPubKey = Utils.publicKeyFromKeyPair(issuerKeyPair);
    let hashBuffer = makeItemHash(issuerPubKey, event);
    {
      event,
      hash: hashBuffer |> Utils.bufToHex,
      issuerPubKey,
      signature: issuerKeyPair |> Bitcoin.ECPair.sign(hashBuffer)
    };
  };
  let append = (issuer, event, log) => {
    let item = event |> makeItem(issuer);
    (item, [item, ...log]);
  };
  let reduce = (reducer, start, log) =>
    log |> List.rev |> List.fold_left(reducer, start);
  module Encode = {
    let ecSig = ecSig => Json.Encode.string(ecSig |> Utils.signatureToString);
    let item = item =>
      Json.Encode.(
        object_([
          ("event", Event.encode(item.event)),
          ("hash", string(item.hash)),
          ("issuerPubKey", string(item.issuerPubKey)),
          ("signature", ecSig(item.signature))
        ])
      );
    let log = Json.Encode.(list(item));
  };
  let encode = Encode.log;
  module Decode = {
    let ecSig = ecSig =>
      ecSig |> Json.Decode.string |> Utils.signatureFromString;
    let item = item =>
      Json.Decode.{
        event: item |> field("event", Event.decode),
        hash: item |> field("hash", string),
        issuerPubKey: item |> field("issuerPubKey", string),
        signature: item |> field("signature", ecSig)
      };
    let log = Json.Decode.(list(item));
  };
  let decode = Decode.log;
};
