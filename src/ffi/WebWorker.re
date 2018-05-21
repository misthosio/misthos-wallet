open Belt;

module type Config = {
  type t;
  type incoming;
  type outgoing;
  let decodeOutgoing: Js.Json.t => outgoing;
  let encodeIncoming: incoming => Js.Json.t;
  let instance: unit => t;
};

type correlationId = string;

type message = {
  .
  "payload": Js.Json.t,
  "correlationId": correlationId,
};

module MakeClient = (Config: Config) => {
  type listener = (. Config.outgoing) => unit;
  let syncListeners: ref(Map.String.t(listener)) = ref(Map.String.empty);
  type t = Config.t;
  [@bs.set]
  external _onMessage : (t, {. "data": message} => unit) => unit =
    "onmessage";
  [@bs.send] external terminate : t => unit = "terminate";
  [@bs.send] external _postMessage : (t, message) => unit = "postMessage";
  let postMessage = (worker, msg) => {
    let msgId = Uuid.v4();
    _postMessage(
      worker,
      {"payload": msg |> Config.encodeIncoming, "correlationId": msgId},
    );
    msgId;
  };
  let postMessageSync = (worker, msg) => {
    let msgId = Uuid.v4();
    let ret =
      Js.Promise.make((~resolve, ~reject as _) =>
        syncListeners := syncListeners^ |. Map.String.set(msgId, resolve)
      );
    _postMessage(
      worker,
      {"payload": msg |> Config.encodeIncoming, "correlationId": msgId},
    );
    ret;
  };
  let handleMessage = (onMessage, msg) => {
    let decodedMsg = msg##payload |> Config.decodeOutgoing;
    let msgId = msg##correlationId;
    switch (syncListeners^ |. Map.String.get(msgId)) {
    | Some(listener) =>
      syncListeners := syncListeners^ |. Map.String.remove(msgId);
      listener(. decodedMsg);
    | _ => ()
    };
    decodedMsg |> onMessage;
  };
  let make = (~onMessage) => {
    let worker = Config.instance();
    worker |. _onMessage(msg => handleMessage(onMessage, msg##data));
    worker;
  };
};
