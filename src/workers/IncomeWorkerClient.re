module Config = {
  include IncomeWorkerMessage;
  type t;
  [@bs.module] [@bs.new]
  external instance : unit => t = "./Income_worker.bs.js";
};

include WebWorker.MakeClient(Config);
