module Message = {
  type exposedAddresses = array(string);
  type txIds = array(string);
  type send =
    | MonitorAddresses(exposedAddresses, txIds);
  type receive =
    | NewTransactionsDetected(list(WalletTypes.transaction));
  let _encodeToSend =
    fun
    | MonitorAddresses(addresses, txIds) => {
        "toMonitor": addresses,
        "knownTxIds": txIds,
      };
  let _decodeToReceived = message =>
    NewTransactionsDetected(
      message##data
      |> Json.Decode.(array(SmartbitClient.decodeTransaction))
      |> Array.to_list,
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

let postMessage = (worker, message) =>
  message |> Message._encodeToSend |> _postMessage(worker);

let workerPath = "/incomeWorker.js";

let make = (~onMessage) => {
  let worker = _makeWorker(workerPath);
  _onmessage(worker, Utils.(Message._decodeToReceived >> onMessage));
  worker;
};
