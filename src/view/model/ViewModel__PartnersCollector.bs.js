// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var Utils = require("../../utils/Utils.bs.js");
var Policy = require("../../application/Policy.bs.js");
var Belt_Map = require("bs-platform/lib/js/belt_Map.js");
var Belt_Set = require("bs-platform/lib/js/belt_Set.js");
var UserInfo = require("../../application/UserInfo.bs.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Js_option = require("bs-platform/lib/js/js_option.js");
var PrimitiveTypes = require("../../application/PrimitiveTypes.bs.js");
var ProcessCollector = require("./ProcessCollector.bs.js");

function currentPartners(param) {
  return Belt_List.reduceU(param[/* partners */1], PrimitiveTypes.UserId[/* emptySet */9], (function (set, param) {
                return Belt_Set.add(set, param[/* userId */0]);
              }));
}

function getPartnerProcess(processId, param) {
  return Belt_Map.get(param[/* partnerProcesses */2], processId);
}

function processesPendingApproval(param) {
  return Belt_List.reduceReverseU(Belt_List.keepU(Belt_List.fromArray(Belt_Map.valuesToArray(param[/* partnerProcesses */2])), (function (prospect) {
                    var match = prospect[/* status */1];
                    return match === 0;
                  })), /* tuple */[
              /* [] */0,
              /* [] */0
            ], (function (param, $$process) {
                var removals = param[1];
                var additions = param[0];
                var match = $$process[/* data */5][/* processType */1] === /* Addition */1;
                if (match) {
                  return /* tuple */[
                          /* :: */[
                            $$process,
                            additions
                          ],
                          removals
                        ];
                } else {
                  return /* tuple */[
                          additions,
                          /* :: */[
                            $$process,
                            removals
                          ]
                        ];
                }
              }));
}

function hasUserLoggedIn(pubKey, userId) {
  if (pubKey !== undefined) {
    return Promise.resolve(true);
  } else {
    return UserInfo.Public[/* read */0](userId).then((function (param) {
                  if (param) {
                    return Promise.resolve(true);
                  } else {
                    return Promise.resolve(false);
                  }
                }));
  }
}

function make(localUser) {
  return /* record */[
          /* localUser */localUser,
          /* partners : [] */0,
          /* partnerProcesses */PrimitiveTypes.ProcessId[/* makeMap */8](/* () */0),
          /* partnerPolicy : Unanimous */0,
          /* everJoinedWallet */PrimitiveTypes.UserId[/* emptySet */9]
        ];
}

function apply($$event, state) {
  var exit = 0;
  switch ($$event.tag | 0) {
    case 0 : 
        return /* record */[
                /* localUser */state[/* localUser */0],
                /* partners */state[/* partners */1],
                /* partnerProcesses */state[/* partnerProcesses */2],
                /* partnerPolicy */Js_option.getWithDefault(Policy.defaultAddPartner, Utils.mapOption((function (p) {
                            return p[/* addPartner */0];
                          }), $$event[0][/* initialPolicies */6])),
                /* everJoinedWallet */state[/* everJoinedWallet */4]
              ];
    case 1 : 
        var proposal = $$event[0];
        return /* record */[
                /* localUser */state[/* localUser */0],
                /* partners */state[/* partners */1],
                /* partnerProcesses */ProcessCollector.addProposal(state[/* localUser */0], proposal, (function (data) {
                        return /* record */[
                                /* userId */data[/* id */1],
                                /* processType : Addition */1,
                                /* hasLoggedIn */hasUserLoggedIn(proposal[/* data */6][/* pubKey */2], proposal[/* data */6][/* id */1]),
                                /* joinedWallet */Belt_Set.has(state[/* everJoinedWallet */4], data[/* id */1])
                              ];
                      }), state[/* partnerProcesses */2]),
                /* partnerPolicy */state[/* partnerPolicy */3],
                /* everJoinedWallet */state[/* everJoinedWallet */4]
              ];
    case 4 : 
        var acceptance = $$event[0];
        var data = acceptance[/* data */2];
        return /* record */[
                /* localUser */state[/* localUser */0],
                /* partners : :: */[
                  /* record */[
                    /* userId */data[/* id */1],
                    /* processId */acceptance[/* processId */0],
                    /* name */undefined,
                    /* canProposeRemoval */PrimitiveTypes.UserId[/* neq */6](data[/* id */1], state[/* localUser */0]),
                    /* hasLoggedIn */hasUserLoggedIn(data[/* pubKey */2], data[/* id */1]),
                    /* joinedWallet */Belt_Set.has(state[/* everJoinedWallet */4], data[/* id */1])
                  ],
                  Belt_List.keepU(state[/* partners */1], (function (param) {
                          return PrimitiveTypes.UserId[/* neq */6](param[/* userId */0], data[/* id */1]);
                        }))
                ],
                /* partnerProcesses */ProcessCollector.addAcceptance(acceptance, state[/* partnerProcesses */2]),
                /* partnerPolicy */state[/* partnerPolicy */3],
                /* everJoinedWallet */state[/* everJoinedWallet */4]
              ];
    case 6 : 
        var partnerId = $$event[0][/* partnerId */0];
        return /* record */[
                /* localUser */state[/* localUser */0],
                /* partners */Belt_List.mapU(state[/* partners */1], (function (partner) {
                        return /* record */[
                                /* userId */partner[/* userId */0],
                                /* processId */partner[/* processId */1],
                                /* name */partner[/* name */2],
                                /* canProposeRemoval */partner[/* canProposeRemoval */3],
                                /* hasLoggedIn */partner[/* hasLoggedIn */4].then((function (known) {
                                        return Promise.resolve(known || PrimitiveTypes.UserId[/* eq */5](partner[/* userId */0], partnerId));
                                      })),
                                /* joinedWallet */partner[/* joinedWallet */5]
                              ];
                      })),
                /* partnerProcesses */state[/* partnerProcesses */2],
                /* partnerPolicy */state[/* partnerPolicy */3],
                /* everJoinedWallet */state[/* everJoinedWallet */4]
              ];
    case 7 : 
        var proposal$1 = $$event[0];
        var partner = Js_option.getExn(Belt_List.getByU(state[/* partners */1], (function (p) {
                    return PrimitiveTypes.UserId[/* eq */5](p[/* userId */0], proposal$1[/* data */6][/* id */0]);
                  })));
        return /* record */[
                /* localUser */state[/* localUser */0],
                /* partners */Belt_List.map(state[/* partners */1], (function (p) {
                        var match = PrimitiveTypes.UserId[/* eq */5](p[/* userId */0], proposal$1[/* data */6][/* id */0]);
                        if (match) {
                          return /* record */[
                                  /* userId */p[/* userId */0],
                                  /* processId */p[/* processId */1],
                                  /* name */p[/* name */2],
                                  /* canProposeRemoval */false,
                                  /* hasLoggedIn */p[/* hasLoggedIn */4],
                                  /* joinedWallet */p[/* joinedWallet */5]
                                ];
                        } else {
                          return p;
                        }
                      })),
                /* partnerProcesses */ProcessCollector.addProposal(state[/* localUser */0], proposal$1, (function (data) {
                        return /* record */[
                                /* userId */data[/* id */0],
                                /* processType : Removal */0,
                                /* hasLoggedIn */partner[/* hasLoggedIn */4],
                                /* joinedWallet */partner[/* joinedWallet */5]
                              ];
                      }), state[/* partnerProcesses */2]),
                /* partnerPolicy */state[/* partnerPolicy */3],
                /* everJoinedWallet */state[/* everJoinedWallet */4]
              ];
    case 2 : 
    case 8 : 
        exit = 1;
        break;
    case 3 : 
    case 9 : 
        exit = 2;
        break;
    case 10 : 
        var acceptance$1 = $$event[0];
        var id = acceptance$1[/* data */2][/* id */0];
        return /* record */[
                /* localUser */state[/* localUser */0],
                /* partners */Belt_List.keep(state[/* partners */1], (function (p) {
                        return PrimitiveTypes.UserId[/* neq */6](p[/* userId */0], id);
                      })),
                /* partnerProcesses */ProcessCollector.addAcceptance(acceptance$1, state[/* partnerProcesses */2]),
                /* partnerPolicy */state[/* partnerPolicy */3],
                /* everJoinedWallet */state[/* everJoinedWallet */4]
              ];
    case 5 : 
    case 11 : 
        exit = 3;
        break;
    case 37 : 
        var custodianId = $$event[0][/* custodianId */1];
        var partner$1 = Js_option.getExn(Belt_List.getByU(state[/* partners */1], (function (partner) {
                    return PrimitiveTypes.UserId[/* eq */5](partner[/* userId */0], custodianId);
                  })));
        return /* record */[
                /* localUser */state[/* localUser */0],
                /* partners */Belt_List.mapU(state[/* partners */1], (function (partner) {
                        return /* record */[
                                /* userId */partner[/* userId */0],
                                /* processId */partner[/* processId */1],
                                /* name */partner[/* name */2],
                                /* canProposeRemoval */partner[/* canProposeRemoval */3],
                                /* hasLoggedIn */partner[/* hasLoggedIn */4],
                                /* joinedWallet */partner[/* joinedWallet */5] || PrimitiveTypes.UserId[/* eq */5](partner[/* userId */0], custodianId)
                              ];
                      })),
                /* partnerProcesses */ProcessCollector.updateData(partner$1[/* processId */1], (function (data) {
                        return /* record */[
                                /* userId */data[/* userId */0],
                                /* processType */data[/* processType */1],
                                /* hasLoggedIn */data[/* hasLoggedIn */2],
                                /* joinedWallet */true
                              ];
                      }), state[/* partnerProcesses */2]),
                /* partnerPolicy */state[/* partnerPolicy */3],
                /* everJoinedWallet */Belt_Set.add(state[/* everJoinedWallet */4], custodianId)
              ];
    default:
      return state;
  }
  switch (exit) {
    case 1 : 
        return /* record */[
                /* localUser */state[/* localUser */0],
                /* partners */state[/* partners */1],
                /* partnerProcesses */ProcessCollector.addRejection(state[/* localUser */0], $$event[0], state[/* partnerProcesses */2]),
                /* partnerPolicy */state[/* partnerPolicy */3],
                /* everJoinedWallet */state[/* everJoinedWallet */4]
              ];
    case 2 : 
        return /* record */[
                /* localUser */state[/* localUser */0],
                /* partners */state[/* partners */1],
                /* partnerProcesses */ProcessCollector.addEndorsement(state[/* localUser */0], $$event[0], state[/* partnerProcesses */2]),
                /* partnerPolicy */state[/* partnerPolicy */3],
                /* everJoinedWallet */state[/* everJoinedWallet */4]
              ];
    case 3 : 
        return /* record */[
                /* localUser */state[/* localUser */0],
                /* partners */state[/* partners */1],
                /* partnerProcesses */ProcessCollector.addDenial($$event[0], state[/* partnerProcesses */2]),
                /* partnerPolicy */state[/* partnerPolicy */3],
                /* everJoinedWallet */state[/* everJoinedWallet */4]
              ];
    
  }
}

function isPartner(id, param) {
  return Belt_List.some(param[/* partners */1], (function (param) {
                return PrimitiveTypes.UserId[/* eq */5](param[/* userId */0], id);
              }));
}

exports.currentPartners = currentPartners;
exports.getPartnerProcess = getPartnerProcess;
exports.processesPendingApproval = processesPendingApproval;
exports.hasUserLoggedIn = hasUserLoggedIn;
exports.make = make;
exports.apply = apply;
exports.isPartner = isPartner;
/* Utils Not a pure module */
