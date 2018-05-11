// Generated by BUCKLESCRIPT VERSION 3.0.0, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var WebWorker = require("../ffi/WebWorker.bs.js");
var WorkerLocalStorage = require("./WorkerLocalStorage.bs.js");
var VentureWorkerMessage = require("./VentureWorkerMessage.bs.js");
var Venture_workerBsJs = require("./Venture_worker.bs.js");

var Config = /* module */[
  /* UnknownMessage */VentureWorkerMessage.UnknownMessage,
  /* encodeIncoming */VentureWorkerMessage.encodeIncoming,
  /* decodeIncoming */VentureWorkerMessage.decodeIncoming,
  /* encodeOutgoing */VentureWorkerMessage.encodeOutgoing,
  /* decodeOutgoing */VentureWorkerMessage.decodeOutgoing
];

var include = WebWorker.MakeClient([
      VentureWorkerMessage.decodeOutgoing,
      (function () {
          return new Venture_workerBsJs();
        })
    ]);

function postMessage(worker, msg) {
  worker.postMessage(VentureWorkerMessage.encodeIncoming(msg));
  return /* () */0;
}

function updateSession(worker) {
  return postMessage(worker, /* UpdateSession */Block.__(0, [WorkerLocalStorage.readBlockstackItemsFromStorage(/* () */0)]));
}

function create(name, worker) {
  return postMessage(worker, /* Create */Block.__(1, [name]));
}

function load(ventureId, worker) {
  return postMessage(worker, /* Load */Block.__(2, [ventureId]));
}

function joinVia(ventureId, userId, worker) {
  return postMessage(worker, /* JoinVia */Block.__(3, [
                ventureId,
                userId
              ]));
}

function proposePartner(worker, ventureId, prospectId) {
  return postMessage(worker, /* ProposePartner */Block.__(4, [
                ventureId,
                prospectId
              ]));
}

function rejectPartner(worker, ventureId, processId) {
  return postMessage(worker, /* RejectPartner */Block.__(5, [
                ventureId,
                processId
              ]));
}

function endorsePartner(worker, ventureId, processId) {
  return postMessage(worker, /* EndorsePartner */Block.__(6, [
                ventureId,
                processId
              ]));
}

function proposePartnerRemoval(worker, ventureId, partnerId) {
  return postMessage(worker, /* ProposePartnerRemoval */Block.__(7, [
                ventureId,
                partnerId
              ]));
}

function rejectPartnerRemoval(worker, ventureId, processId) {
  return postMessage(worker, /* RejectPartnerRemoval */Block.__(8, [
                ventureId,
                processId
              ]));
}

function endorsePartnerRemoval(worker, ventureId, processId) {
  return postMessage(worker, /* EndorsePartnerRemoval */Block.__(9, [
                ventureId,
                processId
              ]));
}

function proposePayout(worker, ventureId, accountIdx, destinations, fee) {
  return postMessage(worker, /* ProposePayout */Block.__(10, [
                ventureId,
                accountIdx,
                destinations,
                fee
              ]));
}

function rejectPayout(worker, ventureId, processId) {
  return postMessage(worker, /* RejectPayout */Block.__(11, [
                ventureId,
                processId
              ]));
}

function endorsePayout(worker, ventureId, processId) {
  return postMessage(worker, /* EndorsePayout */Block.__(12, [
                ventureId,
                processId
              ]));
}

function exposeIncomeAddress(worker, ventureId, accountIdx) {
  return postMessage(worker, /* ExposeIncomeAddress */Block.__(13, [
                ventureId,
                accountIdx
              ]));
}

function make(worker, ventureId) {
  return /* record */[
          /* proposePartner */(function (param) {
              return proposePartner(worker, ventureId, param);
            }),
          /* endorsePartner */(function (param) {
              return endorsePartner(worker, ventureId, param);
            }),
          /* rejectPartner */(function (param) {
              return rejectPartner(worker, ventureId, param);
            }),
          /* proposePartnerRemoval */(function (param) {
              return proposePartnerRemoval(worker, ventureId, param);
            }),
          /* rejectPartnerRemoval */(function (param) {
              return rejectPartnerRemoval(worker, ventureId, param);
            }),
          /* endorsePartnerRemoval */(function (param) {
              return endorsePartnerRemoval(worker, ventureId, param);
            }),
          /* proposePayout */(function (param, param$1, param$2) {
              return proposePayout(worker, ventureId, param, param$1, param$2);
            }),
          /* endorsePayout */(function (param) {
              return endorsePayout(worker, ventureId, param);
            }),
          /* rejectPayout */(function (param) {
              return rejectPayout(worker, ventureId, param);
            }),
          /* exposeIncomeAddress */(function (param) {
              return exposeIncomeAddress(worker, ventureId, param);
            })
        ];
}

var Cmd = /* module */[/* make */make];

var make$1 = include[0];

exports.Config = Config;
exports.make = make$1;
exports.postMessage = postMessage;
exports.updateSession = updateSession;
exports.create = create;
exports.load = load;
exports.joinVia = joinVia;
exports.proposePartner = proposePartner;
exports.rejectPartner = rejectPartner;
exports.endorsePartner = endorsePartner;
exports.proposePartnerRemoval = proposePartnerRemoval;
exports.rejectPartnerRemoval = rejectPartnerRemoval;
exports.endorsePartnerRemoval = endorsePartnerRemoval;
exports.proposePayout = proposePayout;
exports.rejectPayout = rejectPayout;
exports.endorsePayout = endorsePayout;
exports.exposeIncomeAddress = exposeIncomeAddress;
exports.Cmd = Cmd;
/* include Not a pure module */
