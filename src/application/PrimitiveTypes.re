module Base = {
  type t = string;
  external toString : string => 'a = "%identity";
  external fromString : 'a => string = "%identity";
  let encode = id => toString(id) |> Json.Encode.string;
  let decode = id => id |> Json.Decode.string |> fromString;
  let compare = (a, b) => String.compare(toString(a), toString(b));
  let eq = (a, b) => compare(a, b) == 0;
  let neq = (a, b) => compare(a, b) != 0;
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

module UserIdMap = {};

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
