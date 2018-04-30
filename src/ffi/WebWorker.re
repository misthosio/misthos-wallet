module type Config = {
  type t;
  type send;
  type receive;
  let decodeReceive: Js.Json.t => receive;
  let instance: unit => t;
};

module MakeClient = (Config: Config) => {
  type t = Config.t;
  [@bs.set]
  external _onMessage : (t, {. "data": Js.Json.t} => unit) => unit =
    "onmessage";
  [@bs.send] external terminate : t => unit = "";
  [@bs.send] external postMessage : (t, Config.send) => unit = "";
  let make = (~onMessage) => {
    let worker = Config.instance();
    worker |. _onMessage(msg => msg##data |> Config.decodeReceive |> onMessage);
    worker;
  };
};
