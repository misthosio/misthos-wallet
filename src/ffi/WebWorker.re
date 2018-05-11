module type Config = {
  type t;
  type incoming;
  type encodedIncoming;
  type outgoing;
  type encodedOutgoing;
  let decodeOutgoing: encodedOutgoing => outgoing;
  let instance: unit => t;
};

module MakeClient = (Config: Config) => {
  type t = Config.t;
  [@bs.set]
  external _onMessage :
    (t, {. "data": Config.encodedOutgoing} => unit) => unit =
    "onmessage";
  [@bs.send] external terminate : t => unit = "";
  [@bs.send] external postMessage : (t, Config.incoming) => unit = "";
  [@bs.send]
  external postMessageEncoded : (t, Config.encodedIncoming) => unit =
    "postMessage";
  let make = (~onMessage) => {
    let worker = Config.instance();
    worker
    |. _onMessage(msg => msg##data |> Config.decodeOutgoing |> onMessage);
    worker;
  };
};
