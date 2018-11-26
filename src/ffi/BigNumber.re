type t;

module RoundingMode = {
  type t;
  [@bs.module "bignumber.js/bignumber.js"] [@bs.val]
  external ceil : t = "ROUND_CEIL";
  [@bs.module "bignumber.js/bignumber.js"] [@bs.val]
  external floor : t = "ROUND_FLOOR";
};

[@bs.module "bignumber.js/bignumber.js"] [@bs.new]
external make : ([@bs.unwrap] [ | `String(string) | `Float(float)]) => t =
  "BigNumber";

[@bs.send] external times : (t, t) => t = "";

[@bs.send] external dividedBy : (t, t) => t = "";

[@bs.send] external timesFloat : (t, float) => t = "times";

[@bs.send] external dividedByFloat : (t, float) => t = "dividedBy";

[@bs.send] external gte : (t, t) => bool = "";

[@bs.send] external gt : (t, t) => bool = "";

[@bs.send] external plus : (t, t) => t = "";

[@bs.send] external minus : (t, t) => t = "";

[@bs.send] external comparedTo : (t, t) => int = "";

[@bs.send] external integerValue : (t, RoundingMode.t) => t = "";

[@bs.send] external isNaN : t => bool = "";

[@bs.send] external toString : t => string = "";

[@bs.send] external toJSON : t => Js.Json.t = "";

[@bs.send] external toNumber : t => float = "";
