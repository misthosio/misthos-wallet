type t = Node.buffer;

[@bs.val] external makeWithSize : int => t = "Buffer.alloc";

[@bs.val] external compare : (t, t) => int = "Buffer.compare";

[@bs.val]
external fromStringWithEncoding : (Js.String.t, ~encoding: Js.String.t) => t =
  "Buffer.from";

[@bs.send] external toString : t => Js.String.t = "";

[@bs.send.pipe: t]
external toStringWithEncoding : Js.String.t => Js.String.t = "toString";

[@bs.val] external concat : array(t) => t = "Buffer.concat";

[@bs.val]
external byteLength : (Js.String.t, ~encoding: Js.String.t) => int =
  "Buffer.byteLength";

[@bs.get] external length : t => int = "";
