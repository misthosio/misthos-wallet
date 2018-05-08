// Generated by BUCKLESCRIPT VERSION 3.0.0, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Event = require("../events/Event.bs.js");
var Utils = require("../../utils/Utils.bs.js");
var EventLog = require("../events/EventLog.bs.js");
var WalletTypes = require("../wallet/WalletTypes.bs.js");
var Caml_oo_curry = require("bs-platform/lib/js/caml_oo_curry.js");
var CamlinternalOO = require("bs-platform/lib/js/camlinternalOO.js");
var PrimitiveTypes = require("../PrimitiveTypes.bs.js");

var defaultAccountName = "default";

var class_tables = [
  0,
  0,
  0
];

function make(param, param$1, log) {
  var creatorId = param$1[/* creatorId */2];
  if (!class_tables[0]) {
    var $$class = CamlinternalOO.create_table([
          "processCompleted",
          "receive",
          "pendingEvent"
        ]);
    var env = CamlinternalOO.new_variable($$class, "");
    var ids = CamlinternalOO.new_methods_variables($$class, [
          "receive",
          "processCompleted",
          "pendingEvent"
        ], [
          "state",
          "result"
        ]);
    var receive = ids[0];
    var processCompleted = ids[1];
    var pendingEvent = ids[2];
    var state = ids[3];
    var result = ids[4];
    CamlinternalOO.set_methods($$class, /* array */[
          receive,
          (function (self$1, param) {
              var env$1 = self$1[env];
              var $$event = param[/* event */0];
              var match = self$1[state][0];
              var tmp;
              if (typeof match === "number") {
                if (match === 0 && $$event.tag === 1) {
                  var $$event$1 = $$event[0];
                  tmp = PrimitiveTypes.UserId[/* eq */5]($$event$1[/* data */5][/* id */1], env$1[1]) ? /* PartnerProposed */Block.__(0, [$$event$1]) : self$1[state][0];
                } else {
                  tmp = self$1[state][0];
                }
              } else {
                switch (match.tag | 0) {
                  case 0 : 
                      if ($$event.tag === 3) {
                        var partnerProposedEvent = match[0];
                        tmp = PrimitiveTypes.ProcessId[/* eq */5](partnerProposedEvent[/* processId */0], $$event[0][/* processId */0]) ? /* ProposeAccountCreation */Block.__(1, [partnerProposedEvent]) : self$1[state][0];
                      } else {
                        tmp = self$1[state][0];
                      }
                      break;
                  case 1 : 
                      if ($$event.tag === 7) {
                        var $$event$2 = $$event[0];
                        tmp = WalletTypes.AccountIndex[/* eq */7]($$event$2[/* data */5][/* accountIdx */0], WalletTypes.AccountIndex[/* default */9]) ? /* AccountCreationProposed */Block.__(2, [
                              $$event$2[/* processId */0],
                              match[0]
                            ]) : self$1[state][0];
                      } else {
                        tmp = self$1[state][0];
                      }
                      break;
                  case 2 : 
                      tmp = $$event.tag === 9 && PrimitiveTypes.ProcessId[/* eq */5](match[0], $$event[0][/* processId */0]) ? /* ProposeCustodian */Block.__(3, [match[1]]) : self$1[state][0];
                      break;
                  case 3 : 
                      if ($$event.tag === 10) {
                        var $$event$3 = $$event[0];
                        tmp = PrimitiveTypes.UserId[/* eq */5]($$event$3[/* data */5][/* partnerId */0], env$1[1]) ? /* CustodianProposed */Block.__(4, [$$event$3[/* processId */0]]) : self$1[state][0];
                      } else {
                        tmp = self$1[state][0];
                      }
                      break;
                  case 4 : 
                      tmp = $$event.tag === 12 && PrimitiveTypes.ProcessId[/* eq */5](match[0], $$event[0][/* processId */0]) ? /* Complete */1 : self$1[state][0];
                      break;
                  
                }
              }
              self$1[state][0] = tmp;
              var match$1 = self$1[state][0];
              var tmp$1;
              if (typeof match$1 === "number") {
                tmp$1 = match$1 === 0 ? /* Some */[/* tuple */[
                      env$1[0],
                      Event.makePartnerProposed(env$1[1], env$1[1], env$1[2], /* None */0, env$1[3])
                    ]] : /* None */0;
              } else {
                switch (match$1.tag | 0) {
                  case 1 : 
                      tmp$1 = /* Some */[/* tuple */[
                          env$1[0],
                          Event.makeAccountCreationProposed(env$1[1], defaultAccountName, WalletTypes.AccountIndex[/* default */9], env$1[3])
                        ]];
                      break;
                  case 3 : 
                      tmp$1 = /* Some */[/* tuple */[
                          env$1[0],
                          Event.makeCustodianProposed(match$1[0], env$1[1], WalletTypes.AccountIndex[/* default */9], env$1[3])
                        ]];
                      break;
                  default:
                    tmp$1 = /* None */0;
                }
              }
              self$1[result][0] = tmp$1;
              return /* () */0;
            }),
          processCompleted,
          (function (self$1, _) {
              return self$1[state][0] === /* Complete */1;
            }),
          pendingEvent,
          (function (self$1, _) {
              return Utils.mapOption((function (prim) {
                            return Promise.resolve(prim);
                          }), self$1[result][0]);
            })
        ]);
    var env_init = function (env$1) {
      var self = CamlinternalOO.create_object_opt(0, $$class);
      var match = PrimitiveTypes.UserId[/* eq */5](env$1[1], env$1[2]);
      self[state] = [match ? /* ProposePartner */0 : /* Complete */1];
      self[result] = [/* None */0];
      self[env] = env$1[0];
      return self;
    };
    CamlinternalOO.init_class($$class);
    class_tables[0] = env_init;
  }
  var envs_000 = [
    param[/* issuerKeyPair */2],
    creatorId,
    param$1[/* creatorPubKey */3],
    param$1[/* metaPolicy */4]
  ];
  var envs_001 = param[/* userId */0];
  var envs = [
    envs_000,
    envs_001,
    creatorId
  ];
  var $$process = Curry._1(class_tables[0], envs);
  Curry._3(EventLog.reduce, (function (_, item) {
          return Caml_oo_curry.js2(710435299, 1, $$process, item);
        }), /* () */0, log);
  return $$process;
}

exports.defaultAccountName = defaultAccountName;
exports.make = make;
/* Event Not a pure module */
