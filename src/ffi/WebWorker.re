module type Config = {
  type t;
  type incoming;
  type encodedIncoming;
  type outgoing;
  type encodedOutgoing;
  let decodeOutgoing: encodedOutgoing => outgoing;
  let instance: unit => t;
};

type payload('t) = {
  .
  "msg": 't,
  "syncId": string,
};

let emptySyncId = "";

module MakeClient = (Config: Config) => {
  type worker = Config.t;
  type t = {
    worker,
    onMessage: Config.outgoing => unit,
  };
  [@bs.set]
  external _onMessage :
    (worker, {. "data": payload(Config.encodedOutgoing)} => unit) => unit =
    "onmessage";
  [@bs.send] external _terminate : worker => unit = "terminate";
  let terminate = ({worker}) => _terminate(worker);
  [@bs.send]
  external _postMessage : (worker, payload(Config.incoming)) => unit =
    "postMessage";
  let postMessage = ({worker}, msg) =>
    _postMessage(worker, {"msg": msg, "syncId": emptySyncId});
  [@bs.send]
  external _postMessageEncoded :
    (worker, payload(Config.encodedIncoming)) => unit =
    "postMessage";
  let postMessageEncoded = ({worker}, msg) =>
    _postMessageEncoded(worker, {"msg": msg, "syncId": emptySyncId});
  let make = (~onMessage) => {
    let worker = Config.instance();
    worker
    |. _onMessage(msg => msg##data##msg |> Config.decodeOutgoing |> onMessage);
    {worker, onMessage};
  };
};
