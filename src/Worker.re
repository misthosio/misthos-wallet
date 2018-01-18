module Message = {
  type send =
    | RegularlyFetch(array(string));
  type receive =
    | Fetched(list(EventLog.t));
  let _encodeToSend =
    fun
    | RegularlyFetch(links) => links;
  let _decodeToReceived = message =>
    Fetched(
      Array.to_list(message##data)
      |> List.map(Utils.(Json.parseOrRaise >> EventLog.decode))
    );
};

type _worker;

[@bs.new] external _makeWorker : string => _worker = "Worker";

[@bs.set]
external _onmessage : (_worker, Js.t({..}) => unit) => unit = "onmessage";

[@bs.set] external _onerror : (_worker, string => unit) => unit = "onerror";

[@bs.send] external _postMessage : (_worker, 'a) => unit = "postMessage";

[@bs.send] external terminate : _worker => unit = "terminate";

type t = _worker;

let postMessage = worker =>
  Utils.(Message._encodeToSend >> _postMessage(worker));

let make = (~onMessage) => {
  let worker = _makeWorker("worker.js");
  _onmessage(worker, Utils.(Message._decodeToReceived >> onMessage));
  worker;
};
