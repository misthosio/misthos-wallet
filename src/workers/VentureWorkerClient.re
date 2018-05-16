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

let joinVia = (~ventureId, ~userId, worker) =>
  worker |. postMessage(VentureWorkerMessage.JoinVia(ventureId, userId));

let proposePartner = (worker, ventureId, ~prospectId) =>
  worker
  |. postMessage(VentureWorkerMessage.ProposePartner(ventureId, prospectId));

let rejectPartner = (worker, ventureId, ~processId: processId) =>
  worker
  |. postMessage(VentureWorkerMessage.RejectPartner(ventureId, processId));

let endorsePartner = (worker, ventureId, ~processId: processId) =>
  worker
  |. postMessage(VentureWorkerMessage.EndorsePartner(ventureId, processId));

let proposePartnerRemoval = (worker, ventureId, ~partnerId: userId) =>
  worker
  |. postMessage(
       VentureWorkerMessage.ProposePartnerRemoval(ventureId, partnerId),
     );

let rejectPartnerRemoval = (worker, ventureId, ~processId: processId) =>
  worker
  |. postMessage(
       VentureWorkerMessage.RejectPartnerRemoval(ventureId, processId),
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
         destinations,
         fee,
       ),
     );

let rejectPayout = (worker, ventureId, ~processId: processId) =>
  worker
  |. postMessage(VentureWorkerMessage.RejectPayout(ventureId, processId));

let endorsePayout = (worker, ventureId, ~processId: processId) =>
  worker
  |. postMessage(VentureWorkerMessage.EndorsePayout(ventureId, processId));

let exposeIncomeAddress = (worker, ventureId, ~accountIdx) =>
  worker
  |. postMessageSync(
       VentureWorkerMessage.ExposeIncomeAddress(ventureId, accountIdx),
     )
  |> Js.Promise.(
       then_(
         fun
         | VentureWorkerMessage.NewIncomeAddress(_, address) =>
           resolve(address)
         | _ => resolve("BAD"),
       )
     );

module Cmd = {
  type t = {
    proposePartner: (~prospectId: userId) => unit,
    endorsePartner: (~processId: processId) => unit,
    rejectPartner: (~processId: processId) => unit,
    proposePartnerRemoval: (~partnerId: userId) => unit,
    rejectPartnerRemoval: (~processId: processId) => unit,
    endorsePartnerRemoval: (~processId: processId) => unit,
    proposePayout:
      (
        ~accountIdx: accountIdx,
        ~destinations: list((string, BTC.t)),
        ~fee: BTC.t
      ) =>
      unit,
    endorsePayout: (~processId: processId) => unit,
    rejectPayout: (~processId: processId) => unit,
    exposeIncomeAddress: (~accountIdx: accountIdx) => Js.Promise.t(string),
  };
  let make = (worker, ventureId) => {
    proposePartner: proposePartner(worker, ventureId),
    rejectPartner: rejectPartner(worker, ventureId),
    endorsePartner: endorsePartner(worker, ventureId),
    proposePartnerRemoval: proposePartnerRemoval(worker, ventureId),
    rejectPartnerRemoval: rejectPartnerRemoval(worker, ventureId),
    endorsePartnerRemoval: endorsePartnerRemoval(worker, ventureId),
    proposePayout: proposePayout(worker, ventureId),
    rejectPayout: rejectPayout(worker, ventureId),
    endorsePayout: endorsePayout(worker, ventureId),
    exposeIncomeAddress: exposeIncomeAddress(worker, ventureId),
  };
};
