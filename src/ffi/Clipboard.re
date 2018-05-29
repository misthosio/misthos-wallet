type t;

[@bs.module] [@bs.new] external make : string => t = "clipboard";

[@bs.send] external destroy : t => unit = "";

[@bs.send.pipe: t] external on : (string, 'a => unit) => unit = "";
