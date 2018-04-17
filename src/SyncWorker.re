module Message = {
  type send =
    | RegularlyFetch(array(string), EventLog.summary);
  type receive =
    | Fetched(list(Js.Json.t));
  let _encodeToSend =
    fun
    | RegularlyFetch(links, summary) => {
        "links": links,
        "summary": EventLog.encodeSummary(summary),
      };
  let _decodeToReceived = message =>
    Fetched([message##data |> Json.parseOrRaise]);
};

type _worker;

[@bs.new] external _makeWorker : string => _worker = "Worker";

[@bs.set]
external _onmessage : (_worker, Js.t({..}) => unit) => unit = "onmessage";

[@bs.set] external _onerror : (_worker, string => unit) => unit = "onerror";

[@bs.send] external _postMessage : (_worker, 'a) => unit = "postMessage";

[@bs.send] external terminate : _worker => unit = "terminate";

type t = _worker;

let postMessage = (worker, message) =>
  message |> Message._encodeToSend |> _postMessage(worker);

let workerPath = "/syncWorker.js";

let make = (~onMessage) => {
  let worker = _makeWorker(workerPath);
  _onmessage(worker, Utils.(Message._decodeToReceived >> onMessage));
  worker;
};
