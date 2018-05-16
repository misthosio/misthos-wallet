open Belt;

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
  type listener = (. Config.outgoing) => unit;
  let syncListeners: ref(Map.String.t(listener)) = ref(Map.String.empty);
  type t = Config.t;
  [@bs.set]
  external _onMessage :
    (t, {. "data": payload(Config.encodedOutgoing)} => unit) => unit =
    "onmessage";
  [@bs.send] external terminate : t => unit = "terminate";
  [@bs.send]
  external _postMessage : (t, payload(Config.incoming)) => unit =
    "postMessage";
  let postMessage = (worker, msg) =>
    _postMessage(worker, {"msg": msg, "syncId": emptySyncId});
  [@bs.send]
  external _postMessageEncoded : (t, payload(Config.encodedIncoming)) => unit =
    "postMessage";
  let postMessageEncoded = (worker, msg) =>
    _postMessageEncoded(worker, {"msg": msg, "syncId": emptySyncId});
  let postMessageEncodedSync = (worker, msg) => {
    let syncId = Uuid.v4();
    let ret =
      Js.Promise.make((~resolve, ~reject as _) =>
        syncListeners := syncListeners^ |. Map.String.set(syncId, resolve)
      );
    _postMessageEncoded(worker, {"msg": msg, "syncId": syncId});
    ret;
  };
  let handleMessage = (onMessage, msg) => {
    let decodedMsg = msg##msg |> Config.decodeOutgoing;
    let syncId = msg##syncId;
    if (syncId != emptySyncId) {
      switch (syncListeners^ |. Map.String.get(syncId)) {
      | Some(listener) =>
        listener(. decodedMsg);
        syncListeners := syncListeners^ |. Map.String.remove(syncId);
      | _ => ()
      };
    };
    msg##msg |> Config.decodeOutgoing |> onMessage;
  };
  let make = (~onMessage) => {
    let worker = Config.instance();
    worker |. _onMessage(msg => handleMessage(onMessage, msg##data));
    worker;
  };
};
