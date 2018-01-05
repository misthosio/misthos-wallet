module type Encodable = {
  type t;
  let hash: t => string;
  let encode: t => Js.Json.t;
  let decode: Js.Json.t => t;
};

module Make:
  (Event: Encodable) =>
  {
    type t;
    let make: string => t;
    let append: (Event.t, Bitcoin.ECPair.t, t) => t;
    let reduce: (('s, Event.t) => 's, 's, t) => 's;
    let encode: t => Js.Json.t;
    let decode: Js.Json.t => t;
  };
