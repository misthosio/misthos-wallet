type t = Node.buffer;

[@bs.val] external fromStringWithEncoding : (Js.String.t, ~encoding: Js.String.t) => t =
  "Buffer.from";

[@bs.send] external toString : t => Js.String.t = "";

[@bs.send.pipe : t] external toStringWithEncoding : Js.String.t => Js.String.t = "toString";
