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
     )
  |> ignore;

let create = (~name, ~accountSettings, ~initialPolicies, worker) =>
  worker
  |. postMessage(
       VentureWorkerMessage.Create(name, accountSettings, initialPolicies),
     );

let load = (~ventureId, worker) =>
  worker |. postMessage(VentureWorkerMessage.Load(ventureId));

let joinVia = (~ventureId, ~userId, worker) =>
  worker |. postMessage(VentureWorkerMessage.JoinVia(ventureId, userId));

let registerIntegration = (worker, ventureId, ~integrationPubKey) =>
  worker
  |. postMessage(
       VentureWorkerMessage.RegisterIntegration(ventureId, integrationPubKey),
     );

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

let submitCustodianKeyChain = (worker, ventureId, ~keyChain) =>
  worker
  |. postMessage(
       VentureWorkerMessage.SubmitCustodianKeyChain(ventureId, keyChain),
     );

let proposePayout =
    (worker, ventureId, ~accountIdx: accountIdx, ~payoutTx, ~signatures) =>
  worker
  |. postMessage(
       VentureWorkerMessage.ProposePayout(
         ventureId,
         accountIdx,
         payoutTx,
         signatures,
       ),
     );

let rejectPayout = (worker, ventureId, ~processId: processId) =>
  worker
  |. postMessage(VentureWorkerMessage.RejectPayout(ventureId, processId));

let endorsePayout = (worker, ventureId, ~signatures, ~processId: processId) =>
  worker
  |. postMessage(
       VentureWorkerMessage.EndorsePayout(ventureId, signatures, processId),
     );

let signPayout = (worker, ventureId, ~signatures, ~processId: processId) =>
  worker
  |. postMessage(
       VentureWorkerMessage.SignPayout(ventureId, signatures, processId),
     );

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
    registerIntegration:
      (~integrationPubKey: string) => WebWorker.correlationId,
    proposePartner: (~prospectId: userId) => WebWorker.correlationId,
    endorsePartner: (~processId: processId) => WebWorker.correlationId,
    rejectPartner: (~processId: processId) => WebWorker.correlationId,
    proposePartnerRemoval: (~partnerId: userId) => WebWorker.correlationId,
    rejectPartnerRemoval: (~processId: processId) => WebWorker.correlationId,
    endorsePartnerRemoval: (~processId: processId) => WebWorker.correlationId,
    submitCustodianKeyChain:
      (~keyChain: CustodianKeyChain.public) => WebWorker.correlationId,
    proposePayout:
      (
        ~accountIdx: accountIdx,
        ~payoutTx: PayoutTransaction.t,
        ~signatures: array(option((string, string)))
      ) =>
      WebWorker.correlationId,
    endorsePayout:
      (
        ~signatures: array(option((string, string))),
        ~processId: processId
      ) =>
      WebWorker.correlationId,
    signPayout:
      (
        ~signatures: array(option((string, string))),
        ~processId: processId
      ) =>
      WebWorker.correlationId,
    rejectPayout: (~processId: processId) => WebWorker.correlationId,
    exposeIncomeAddress: (~accountIdx: accountIdx) => Js.Promise.t(string),
  };
  let make = (worker, ventureId) => {
    registerIntegration: registerIntegration(worker, ventureId),
    proposePartner: proposePartner(worker, ventureId),
    rejectPartner: rejectPartner(worker, ventureId),
    endorsePartner: endorsePartner(worker, ventureId),
    proposePartnerRemoval: proposePartnerRemoval(worker, ventureId),
    rejectPartnerRemoval: rejectPartnerRemoval(worker, ventureId),
    endorsePartnerRemoval: endorsePartnerRemoval(worker, ventureId),
    submitCustodianKeyChain: submitCustodianKeyChain(worker, ventureId),
    proposePayout: proposePayout(worker, ventureId),
    rejectPayout: rejectPayout(worker, ventureId),
    endorsePayout: endorsePayout(worker, ventureId),
    signPayout: signPayout(worker, ventureId),
    exposeIncomeAddress: exposeIncomeAddress(worker, ventureId),
  };
};
