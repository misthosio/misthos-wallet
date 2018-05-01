module Config = {
  include PersistWorkerMessage;
  type t;
  [@bs.module] [@bs.new]
  external instance : unit => t = "./Persist_worker.bs.js";
};

include WebWorker.MakeClient(Config);

let make = (~onMessage) => {
  let worker = make(~onMessage);
  worker
  |. postMessage(
       PersistWorkerMessage.InitializeLocalStorage(
         WorkerLocalStorage.readBlockstackItemsFromStorage(),
       ),
     );
  worker;
};
