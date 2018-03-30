type t;

[@bs.module "bignumber.js"] [@bs.new]
external make : ([@bs.unwrap] [ | `String(string) | `Float(float)]) => t =
  "BigNumber";

[@bs.send.pipe : t] external times : t => t = "";

[@bs.send.pipe : t] external timesFloat : float => t = "times";

[@bs.send.pipe : t] external gte : t => bool = "";

[@bs.send.pipe : t] external gt : t => bool = "";

[@bs.send.pipe : t] external plus : t => t = "";

[@bs.send.pipe : t] external minus : t => t = "";

[@bs.send.pipe : t] external comparedTo : t => int = "";

[@bs.send] external toString : t => string = "";

[@bs.send] external toNumber : t => float = "";
