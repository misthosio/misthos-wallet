module type Encodable = {
  type t;
  let encode: t => string;
  let decode: string => t;
};

module Make:
  (Event: Encodable) =>
  {
    type t;
    let make: unit => t;
    let append: (Event.t, Bitcoin.ECPair.t, t) => t;
    let reduce: (('s, Event.t) => 's, 's, t) => 's;
    let merge: (t, list(t)) => t;
    let encode: t => string;
    let decode: string => t;
  };
