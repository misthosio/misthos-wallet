module Base = {
  type t = string;
  external toString : string => 'a = "%identity";
  external fromString : 'a => string = "%identity";
  let encode = id => toString(id) |> Json.Encode.string;
  let decode = id => id |> Json.Decode.string |> fromString;
  let compare = (a, b) => String.compare(toString(a), toString(b));
  let eq = (a, b) => compare(a, b) == 0;
  let neq = (a, b) => compare(a, b) != 0;
  type parent = t;
  module Comparator =
    Belt.Id.MakeComparableU(
      {
        type t = parent;
        let cmp = (. pA, pB) => compare(pA, pB);
      },
    );
  type map('v) = Belt.Map.t(Comparator.t, 'v, Comparator.identity);
};

module type PrimitiveType = {
  type t;
  let toString: t => string;
  let fromString: string => t;
  let encode: t => Js.Json.t;
  let decode: Js.Json.t => t;
  let compare: (t, t) => int;
  let eq: (t, t) => bool;
  let neq: (t, t) => bool;
  type parent = t;
  module Comparator: {
    type identity;
    type t = parent;
    let cmp: Belt.Id.cmp(t, identity);
  };
  type map('v) = Belt.Map.t(t, 'v, Comparator.identity);
};

module VentureId = {
  include Base;
  let make = Uuid.v4;
};

type ventureId = VentureId.t;

module UserId = {
  include Base;
};

type userId = UserId.t;

module ProcessId = {
  include Base;
  let make = Uuid.v4;
};

type processId = ProcessId.t;

module LabelId = {
  include Base;
  let make = Uuid.v4;
};

type labelId = LabelId.t;
