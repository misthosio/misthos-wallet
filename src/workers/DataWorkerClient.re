module Config = {
  include DataWorkerMessage;
  type t;
  [@bs.module] [@bs.new] external instance : unit => t = "./Data_worker.bs.js";
};

include WebWorker.MakeClient(Config);
