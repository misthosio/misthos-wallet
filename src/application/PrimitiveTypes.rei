module VentureId: {
  type t;
  let make: unit => t;
  let toString: t => string;
  let fromString: string => t;
  let encode: t => Js.Json.t;
  let decode: Js.Json.t => t;
};

type ventureId = VentureId.t;

module UserId: {
  type t;
  let fromString: string => t;
  let toString: t => string;
  let encode: t => Js.Json.t;
  let decode: Js.Json.t => t;
};

type userId = UserId.t;

module ProcessId: {
  type t;
  let make: unit => t;
  let toString: t => string;
  let encode: t => Js.Json.t;
  let decode: Js.Json.t => t;
};

type processId = ProcessId.t;

module LabelId: {
  type t;
  let make: unit => t;
  let toString: t => string;
  let encode: t => Js.Json.t;
  let decode: Js.Json.t => t;
};

type labelId = LabelId.t;
