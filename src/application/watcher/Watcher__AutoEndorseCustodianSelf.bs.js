// Generated by BUCKLESCRIPT VERSION 4.0.5, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Event = require("../events/Event.bs.js");
var EventLog = require("../events/EventLog.bs.js");
var Caml_oo_curry = require("bs-platform/lib/js/caml_oo_curry.js");
var CamlinternalOO = require("bs-platform/lib/js/camlinternalOO.js");
var PrimitiveTypes = require("../PrimitiveTypes.bs.js");

var class_tables = [
  0,
  0,
  0
];

function make(param, param$1, log) {
  var partnerId = param$1[/* data */6][/* partnerId */0];
  var custodianProcessId = param$1[/* processId */0];
  var userId = param[/* userId */0];
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
        ], ["state"]);
    var receive = ids[0];
    var processCompleted = ids[1];
    var pendingEvent = ids[2];
    var state = ids[3];
    CamlinternalOO.set_methods($$class, /* array */[
          receive,
          (function (self$1, param) {
              var env$1 = self$1[env];
              var $$event = param[/* event */0];
              var match = PrimitiveTypes.UserId[/* eq */5](env$1[2], env$1[0]);
              var tmp;
              if (match) {
                switch ($$event.tag | 0) {
                  case 16 : 
                      if (PrimitiveTypes.UserId[/* eq */5]($$event[0][/* proposerId */4], env$1[0])) {
                        var init = self$1[state][0];
                        tmp = /* record */[
                          /* pendingEvent */init[/* pendingEvent */0],
                          /* completed */true
                        ];
                      } else {
                        tmp = self$1[state][0];
                      }
                      break;
                  case 18 : 
                      var match$1 = $$event[0];
                      tmp = PrimitiveTypes.ProcessId[/* eq */5](match$1[/* processId */0], env$1[1]) && PrimitiveTypes.UserId[/* eq */5](match$1[/* supporterId */1], env$1[0]) ? /* record */[
                          /* pendingEvent */undefined,
                          /* completed */true
                        ] : self$1[state][0];
                      break;
                  case 19 : 
                      tmp = PrimitiveTypes.ProcessId[/* eq */5]($$event[0][/* processId */0], env$1[1]) ? /* record */[
                          /* pendingEvent */undefined,
                          /* completed */true
                        ] : self$1[state][0];
                      break;
                  default:
                    tmp = self$1[state][0];
                }
              } else {
                tmp = /* record */[
                  /* pendingEvent */undefined,
                  /* completed */true
                ];
              }
              self$1[state][0] = tmp;
              return /* () */0;
            }),
          processCompleted,
          (function (self$1, _) {
              return self$1[state][0][/* completed */1];
            }),
          pendingEvent,
          (function (self$1, _) {
              return self$1[state][0][/* pendingEvent */0];
            })
        ]);
    var env_init = function (env$1) {
      var self = CamlinternalOO.create_object_opt(0, $$class);
      var match = PrimitiveTypes.UserId[/* eq */5](env$1[4], env$1[1]);
      self[state] = /* record */[/* contents : record */[
          /* pendingEvent */match ? /* tuple */[
              env$1[2],
              Event.makeCustodianEndorsed(env$1[3], env$1[1])
            ] : undefined,
          /* completed */false
        ]];
      self[env] = env$1[0];
      return self;
    };
    CamlinternalOO.init_class($$class);
    class_tables[0] = env_init;
  }
  var envs_000 = [
    userId,
    custodianProcessId,
    partnerId
  ];
  var envs_002 = param[/* issuerKeyPair */2];
  var envs = [
    envs_000,
    userId,
    envs_002,
    custodianProcessId,
    partnerId
  ];
  var $$process = Curry._1(class_tables[0], envs);
  Curry._3(EventLog.reduce, (function (_, item) {
          return Caml_oo_curry.js2(710435299, 1, $$process, item);
        }), /* () */0, log);
  return $$process;
}

exports.make = make;
/* Event Not a pure module */
