type t;

type document;

[@bs.send]
external getElementById : (document, string) => Dom.element = "getElementById";

[@bs.val] external doc : document = "document";

[@bs.module] [@bs.new]
external _make : (string, {. "container": Dom.element}) => t = "clipboard";

let make = (selector, elementName) =>
  _make(selector, {"container": getElementById(doc, elementName)});

[@bs.send] external destroy : t => unit = "";

[@bs.send.pipe: t] external on : (string, 'a => unit) => unit = "";
