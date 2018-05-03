module Config = {
  include PersistWorkerMessage;
  type t;
  [@bs.module] [@bs.new]
  external instance : unit => t = "./Persist_worker.bs.js";
};

include WebWorker.MakeClient(Config);

let updateSession = (userId, worker) =>
  worker
  |. postMessage(
       PersistWorkerMessage.UpdateSession(
         userId,
         WorkerLocalStorage.readBlockstackItemsFromStorage(),
       ),
     );

let ventureMessage = (msg, worker) =>
  PersistWorkerMessage.VentureWorkerMessage(
    msg |> VentureWorkerMessage.encodeReceive,
  )
  |> postMessage(worker);
