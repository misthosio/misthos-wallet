module type Encodable = {
  type t;
  let encode: t => Js.Json.t;
  let decode: Js.Json.t => t;
};

module Make:
  (Event: Encodable) =>
  {
    type t;
    type summary = {knownItems: list(string)};
    type item = {
      event: Event.t,
      hash: string,
      issuerPubKey: string,
      signature: Bitcoin.ECSignature.t,
    };
    let make: unit => t;
    let append: (Bitcoin.ECPair.t, Event.t, t) => (item, t);
    let appendItem: (item, t) => t;
    let reduce: (('s, item) => 's, 's, t) => 's;
    let findNewItems: (list(t), t) => list(item);
    let length: t => int;
    let encode: t => Js.Json.t;
    let decode: Js.Json.t => t;
    let getSummary: t => summary;
    let encodeSummary: summary => Js.Json.t;
    let decodeSummary: Js.Json.t => summary;
  };
