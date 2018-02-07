module Helper = {
  external pack : string => 'a = "%identity";
  external unpack : 'a => string = "%identity";
  let encode = id => unpack(id) |> Json.Encode.string;
  let decode = id => id |> Json.Decode.string |> pack;
};

module VentureId = {
  type t = string;
  let make = Uuid.v4;
  let toString = Helper.unpack;
  let fromString = Helper.unpack;
  let encode = Helper.encode;
  let decode = Helper.decode;
};

type ventureId = VentureId.t;

module UserId = {
  type t = string;
  let fromString = Helper.pack;
  let toString = Helper.unpack;
  let encode = Helper.encode;
  let decode = Helper.decode;
};

type userId = UserId.t;

module ProcessId = {
  type t = string;
  let make = Uuid.v4;
  let toString = Helper.unpack;
  let encode = Helper.encode;
  let decode = Helper.decode;
};

type processId = ProcessId.t;

module LabelId = {
  type t = string;
  let make = Uuid.v4;
  let toString = Helper.unpack;
  let encode = Helper.encode;
  let decode = Helper.decode;
};

type labelId = LabelId.t;
