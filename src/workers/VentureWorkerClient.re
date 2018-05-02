open PrimitiveTypes;

open WalletTypes;

module Config = {
  include VentureWorkerMessage;
  type t;
  [@bs.module] [@bs.new]
  external instance : unit => t = "./Venture_worker.bs.js";
};

include WebWorker.MakeClient(Config);

let updateSession = worker =>
  worker
  |. postMessage(
       VentureWorkerMessage.UpdateSession(
         WorkerLocalStorage.readBlockstackItemsFromStorage(),
       ),
     );

let create = (~name, worker) =>
  worker |. postMessage(VentureWorkerMessage.Create(name));

let load = (~ventureId, worker) =>
  worker |. postMessage(VentureWorkerMessage.Load(ventureId));

let proposePartner = (worker, ~prospectId) => ();

let endorsePartner = (worker, ~processId: processId) => ();

let proposePartnerRemoval = (worker, ~partnerId: userId) => ();

let endorsePartnerRemoval = (worker, ~processId: processId) => ();

let proposePayout =
    (
      worker,
      ~accountIdx: accountIdx,
      ~destinations: list((string, BTC.t)),
      ~fee: BTC.t,
    ) =>
  ();

let endorsePayout = (worker, ~processId: processId) => ();

module Cmd = {
  type t = {
    proposePartner: (~prospectId: string) => unit,
    endorsePartner: (~processId: processId) => unit,
    proposePartnerRemoval: (~partnerId: userId) => unit,
    endorsePartnerRemoval: (~processId: processId) => unit,
    proposePayout:
      (
        ~accountIdx: accountIdx,
        ~destinations: list((string, BTC.t)),
        ~fee: BTC.t
      ) =>
      unit,
    endorsePayout: (~processId: processId) => unit,
  };
  let make = (worker, _ventureId) => {
    proposePartner: proposePartner(worker),
    endorsePartner: endorsePartner(worker),
    proposePartnerRemoval: proposePartnerRemoval(worker),
    endorsePartnerRemoval: endorsePartnerRemoval(worker),
    proposePayout: proposePayout(worker),
    endorsePayout: endorsePayout(worker),
  };
};
