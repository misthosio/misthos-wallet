open Belt;

module type Config = {
  type t;
  type incoming;
  type outgoing;
  let decodeOutgoing: Js.Json.t => outgoing;
  let encodeIncoming: incoming => Js.Json.t;
  let instance: unit => t;
};

type payload = {
  .
  "msg": Js.Json.t,
  "syncId": string,
};

let emptySyncId = "";

module MakeClient = (Config: Config) => {
  type listener = (. Config.outgoing) => unit;
  let syncListeners: ref(Map.String.t(listener)) = ref(Map.String.empty);
  type t = Config.t;
  [@bs.set]
  external _onMessage : (t, {. "data": payload} => unit) => unit =
    "onmessage";
  [@bs.send] external terminate : t => unit = "terminate";
  [@bs.send] external _postMessage : (t, payload) => unit = "postMessage";
  let postMessage = (worker, msg) =>
    _postMessage(
      worker,
      {"msg": msg |> Config.encodeIncoming, "syncId": emptySyncId},
    );
  let postMessageSync = (worker, msg) => {
    let syncId = Uuid.v4();
    let ret =
      Js.Promise.make((~resolve, ~reject as _) =>
        syncListeners := syncListeners^ |. Map.String.set(syncId, resolve)
      );
    _postMessage(
      worker,
      {"msg": msg |> Config.encodeIncoming, "syncId": syncId},
    );
    ret;
  };
  let handleMessage = (onMessage, msg) => {
    let decodedMsg = msg##msg |> Config.decodeOutgoing;
    let syncId = msg##syncId;
    if (syncId != emptySyncId) {
      switch (syncListeners^ |. Map.String.get(syncId)) {
      | Some(listener) =>
        syncListeners := syncListeners^ |. Map.String.remove(syncId);
        listener(. decodedMsg);
      | _ => ()
      };
    };
    decodedMsg |> onMessage;
  };
  let make = (~onMessage) => {
    let worker = Config.instance();
    worker |. _onMessage(msg => handleMessage(onMessage, msg##data));
    worker;
  };
};
