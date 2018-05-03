module Config = {
  include SyncWorkerMessage;
  type t;
  [@bs.module] [@bs.new] external instance : unit => t = "./Sync_worker.bs.js";
};

include WebWorker.MakeClient(Config);

let updateSession = worker =>
  worker
  |. postMessage(
       SyncWorkerMessage.UpdateSession(
         WorkerLocalStorage.readBlockstackItemsFromStorage(),
       ),
     );
