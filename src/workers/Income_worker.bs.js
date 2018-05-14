// Generated by BUCKLESCRIPT VERSION 3.0.0, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Event = require("../application/events/Event.bs.js");
var Utils = require("../utils/Utils.bs.js");
var Network = require("../application/wallet/Network.bs.js");
var Session = require("../application/Session.bs.js");
var Venture = require("../application/Venture.bs.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var EventLog = require("../application/events/EventLog.bs.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var WorkerUtils = require("./WorkerUtils.bs.js");
var Belt_SetString = require("bs-platform/lib/js/belt_SetString.js");
var PrimitiveTypes = require("../application/PrimitiveTypes.bs.js");
var AddressCollector = require("../application/wallet/AddressCollector.bs.js");
var WorkerLocalStorage = require("./WorkerLocalStorage.bs.js");
var VentureWorkerMessage = require("./VentureWorkerMessage.bs.js");

(( self.localStorage = require("./fakeLocalStorage").localStorage ));

(( self.window = { localStorage: self.localStorage , location: { origin: self.origin } } ));

function postMessage$1(msg) {
  postMessage(VentureWorkerMessage.encodeIncoming(msg));
  return /* () */0;
}

function logMessage(msg) {
  console.log("[Income Worker] - " + msg);
  return /* () */0;
}

function scanTransactions(param) {
  var txIds = param[1];
  var addresses = param[0];
  return Network.transactionInputs(addresses[/* network */0])(addresses[/* exposedAddresses */2]).then((function (utxos) {
                return Promise.resolve(/* tuple */[
                            txIds,
                            utxos
                          ]);
              }));
}

var findAddressesAndTxIds = Curry._2(EventLog.reduce, (function (param, param$1) {
        var $$event = param$1[/* event */0];
        var txIds = param[1];
        var addresses = AddressCollector.apply($$event, param[0]);
        if ($$event.tag === 33) {
          var match = $$event[0];
          return /* tuple */[
                  addresses,
                  Belt_SetString.add(txIds, match[/* txId */2] + String(match[/* txOutputN */3]))
                ];
        } else {
          return /* tuple */[
                  addresses,
                  txIds
                ];
        }
      }), /* tuple */[
      AddressCollector.make(/* () */0),
      Belt_SetString.empty
    ]);

function detectIncomeFromVenture(ventureId) {
  logMessage("Detecting income for venture '" + (PrimitiveTypes.VentureId[/* toString */0](ventureId) + "'"));
  return WorkerUtils.loadVenture(ventureId).then((function (eventLog) {
                  return scanTransactions(Curry._1(findAddressesAndTxIds, eventLog));
                })).then((function (param) {
                var knownTxs = param[0];
                var events = Belt_List.keepMapU(param[1], (function (utxo) {
                        var txOutId = utxo[/* txId */0] + String(utxo[/* txOutputN */1]);
                        var match = Belt_SetString.has(knownTxs, txOutId);
                        if (match) {
                          return /* None */0;
                        } else {
                          return /* Some */[Event.IncomeDetected[/* make */0](utxo[/* txOutputN */1], utxo[/* coordinates */6], utxo[/* address */2], utxo[/* txId */0], utxo[/* value */3])];
                        }
                      }));
                return Promise.resolve(events ? (postMessage(VentureWorkerMessage.encodeIncoming(/* IncomeDetected */Block.__(14, [
                                          ventureId,
                                          events
                                        ]))), /* () */0) : /* () */0);
              }));
}

function detectIncomeFromAll() {
  return Session.getCurrentSession(/* () */0).then((function (param) {
                  if (typeof param === "number") {
                    return Promise.resolve(/* () */0);
                  } else {
                    return Venture.Index[/* load */0](/* () */0).then((function (index) {
                                  return Promise.resolve(Belt_List.forEach(index, (function (param) {
                                                    detectIncomeFromVenture(param[/* id */0]);
                                                    return /* () */0;
                                                  })));
                                }));
                  }
                })).catch((function (err) {
                logMessage("Error while syncing:");
                console.log(err);
                return Promise.resolve(/* () */0);
              }));
}

function handleMsg(param) {
  logMessage("Handling 'UpdateSession'");
  WorkerLocalStorage.setBlockstackItems(param[0]);
  detectIncomeFromAll(/* () */0);
  return setInterval((function () {
                detectIncomeFromAll(/* () */0);
                return /* () */0;
              }), 5000);
}

var intervalId = [/* None */0];

self.onmessage = (function (msg) {
    var newIntervalid = handleMsg(msg.data);
    Utils.mapOption((function (id) {
            if (Caml_obj.caml_notequal(newIntervalid, id)) {
              clearInterval(id);
              return /* () */0;
            } else {
              return 0;
            }
          }), intervalId[0]);
    intervalId[0] = /* Some */[newIntervalid];
    return /* () */0;
  });

var Message = 0;

var fiveSecondsInMilliseconds = 5000;

var syncInterval = 5000;

exports.Message = Message;
exports.postMessage = postMessage$1;
exports.logMessage = logMessage;
exports.scanTransactions = scanTransactions;
exports.findAddressesAndTxIds = findAddressesAndTxIds;
exports.detectIncomeFromVenture = detectIncomeFromVenture;
exports.detectIncomeFromAll = detectIncomeFromAll;
exports.fiveSecondsInMilliseconds = fiveSecondsInMilliseconds;
exports.syncInterval = syncInterval;
exports.handleMsg = handleMsg;
exports.intervalId = intervalId;
/*  Not a pure module */
