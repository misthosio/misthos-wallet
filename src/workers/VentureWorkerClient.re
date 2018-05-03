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

let proposePartner = (worker, ventureId, ~prospectId) =>
  worker
  |. postMessage(VentureWorkerMessage.ProposePartner(ventureId, prospectId));

let endorsePartner = (worker, ventureId, ~processId: processId) =>
  worker
  |. postMessage(VentureWorkerMessage.EndorsePartner(ventureId, processId));

let proposePartnerRemoval = (worker, ventureId, ~partnerId: userId) =>
  worker
  |. postMessage(
       VentureWorkerMessage.ProposePartnerRemoval(ventureId, partnerId),
     );

let endorsePartnerRemoval = (worker, ventureId, ~processId: processId) =>
  worker
  |. postMessage(
       VentureWorkerMessage.EndorsePartnerRemoval(ventureId, processId),
     );

let proposePayout =
    (
      worker,
      ventureId,
      ~accountIdx: accountIdx,
      ~destinations: list((string, BTC.t)),
      ~fee: BTC.t,
    ) =>
  worker
  |. postMessage(
       VentureWorkerMessage.ProposePayout(
         ventureId,
         accountIdx,
         destinations |> List.map(((a, btc)) => (a, btc |> BTC.encode)),
         fee |> BTC.encode,
       ),
     );

let endorsePayout = (worker, ventureId, ~processId: processId) =>
  worker
  |. postMessage(VentureWorkerMessage.EndorsePayout(ventureId, processId));

let exposeIncomeAddress = (worker, ventureId, ~accountIdx) =>
  worker
  |. postMessage(
       VentureWorkerMessage.ExposeIncomeAddress(ventureId, accountIdx),
     );

module Cmd = {
  type t = {
    proposePartner: (~prospectId: userId) => unit,
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
    exposeIncomeAddress: (~accountIdx: accountIdx) => unit,
  };
  let make = (worker, ventureId) => {
    proposePartner: proposePartner(worker, ventureId),
    endorsePartner: endorsePartner(worker, ventureId),
    proposePartnerRemoval: proposePartnerRemoval(worker, ventureId),
    endorsePartnerRemoval: endorsePartnerRemoval(worker, ventureId),
    proposePayout: proposePayout(worker, ventureId),
    endorsePayout: endorsePayout(worker, ventureId),
    exposeIncomeAddress: exposeIncomeAddress(worker, ventureId),
  };
};
