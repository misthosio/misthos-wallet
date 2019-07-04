type t;

module RoundingMode = {
  type t;
  [@bs.module "bignumber.js/bignumber.js"] [@bs.val]
  external ceil: t = "ROUND_CEIL";
  [@bs.module "bignumber.js/bignumber.js"] [@bs.val]
  external floor: t = "ROUND_FLOOR";
};

[@bs.module "bignumber.js/bignumber.js"] [@bs.new]
external make: ([@bs.unwrap] [ | `String(string) | `Float(float)]) => t =
  "BigNumber";

[@bs.send] external times: (t, t) => t = "times";

[@bs.send] external dividedBy: (t, t) => t = "dividedBy";

[@bs.send] external timesFloat: (t, float) => t = "times";

[@bs.send] external dividedByFloat: (t, float) => t = "dividedBy";

[@bs.send] external gte: (t, t) => bool = "gte";

[@bs.send] external gt: (t, t) => bool = "gt";

[@bs.send] external plus: (t, t) => t = "plus";

[@bs.send] external minus: (t, t) => t = "minus";

[@bs.send] external comparedTo: (t, t) => int = "comparedTo";

[@bs.send] external integerValue: (t, RoundingMode.t) => t = "integerValue";

[@bs.send] external isNaN: t => bool = "isNaN";

[@bs.send] external toString: t => string = "toString";

[@bs.send] external toJSON: t => Js.Json.t = "toJSON";

[@bs.send] external toNumber: t => float = "toNumber";
