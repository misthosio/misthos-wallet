// Generated by BUCKLESCRIPT VERSION 3.0.0, PLEASE EDIT WITH CARE
'use strict';

var Utils = require("../../utils/Utils.bs.js");
var Belt_Map = require("bs-platform/lib/js/belt_Map.js");
var Belt_Set = require("bs-platform/lib/js/belt_Set.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var PrimitiveTypes = require("../../application/PrimitiveTypes.bs.js");

function getProspect(processId, param) {
  return Belt_Map.getExn(param[/* prospects */2], processId);
}

function prospectsPendingApproval(param) {
  return Belt_List.keepU(Belt_List.fromArray(Belt_Map.valuesToArray(param[/* prospects */2])), (function (prospect) {
                var match = prospect[/* processStatus */3];
                if (match) {
                  return true;
                } else {
                  return false;
                }
              }));
}

function make(localUser) {
  return /* record */[
          /* localUser */localUser,
          /* partners : [] */0,
          /* prospects */PrimitiveTypes.ProcessId[/* makeMap */8](/* () */0),
          /* partnerPolicy : Unanimous */0
        ];
}

function apply($$event, state) {
  switch ($$event.tag | 0) {
    case 0 : 
        return /* record */[
                /* localUser */state[/* localUser */0],
                /* partners */state[/* partners */1],
                /* prospects */state[/* prospects */2],
                /* partnerPolicy */$$event[0][/* metaPolicy */4]
              ];
    case 1 : 
        var match = $$event[0];
        var supporterId = match[/* supporterId */4];
        var eligibleWhenProposing = match[/* eligibleWhenProposing */3];
        var processId = match[/* processId */0];
        return /* record */[
                /* localUser */state[/* localUser */0],
                /* partners */state[/* partners */1],
                /* prospects */Belt_Map.set(state[/* prospects */2], processId, /* record */[
                      /* processId */processId,
                      /* userId */match[/* data */6][/* id */1],
                      /* processType : Addition */1,
                      /* processStatus : InProgress */1,
                      /* voters */Belt_List.mapU(Belt_Set.toList(eligibleWhenProposing), (function (userId) {
                              var match = PrimitiveTypes.UserId[/* eq */5](supporterId, userId);
                              return /* record */[
                                      /* userId */userId,
                                      /* voteStatus */match ? /* Endorsed */1 : /* Pending */0
                                    ];
                            })),
                      /* canEndorse */PrimitiveTypes.UserId[/* neq */6](supporterId, state[/* localUser */0]) && Belt_Set.has(eligibleWhenProposing, state[/* localUser */0]),
                      /* canReject */PrimitiveTypes.UserId[/* neq */6](supporterId, state[/* localUser */0]) && Belt_Set.has(eligibleWhenProposing, state[/* localUser */0])
                    ]),
                /* partnerPolicy */state[/* partnerPolicy */3]
              ];
    case 2 : 
        var match$1 = $$event[0];
        var rejectorId = match$1[/* rejectorId */1];
        return /* record */[
                /* localUser */state[/* localUser */0],
                /* partners */state[/* partners */1],
                /* prospects */Belt_Map.update(state[/* prospects */2], match$1[/* processId */0], (function (param) {
                        return Utils.mapOption((function (prospect) {
                                      return /* record */[
                                              /* processId */prospect[/* processId */0],
                                              /* userId */prospect[/* userId */1],
                                              /* processType */prospect[/* processType */2],
                                              /* processStatus */prospect[/* processStatus */3],
                                              /* voters */Belt_List.mapU(prospect[/* voters */4], (function (param) {
                                                      var userId = param[/* userId */0];
                                                      var match = PrimitiveTypes.UserId[/* eq */5](userId, rejectorId);
                                                      if (match) {
                                                        return /* record */[
                                                                /* userId */userId,
                                                                /* voteStatus : Rejected */2
                                                              ];
                                                      } else {
                                                        return /* record */[
                                                                /* userId */userId,
                                                                /* voteStatus */param[/* voteStatus */1]
                                                              ];
                                                      }
                                                    })),
                                              /* canEndorse */prospect[/* canEndorse */5] && PrimitiveTypes.UserId[/* neq */6](rejectorId, state[/* localUser */0]),
                                              /* canReject */prospect[/* canReject */6] && PrimitiveTypes.UserId[/* neq */6](rejectorId, state[/* localUser */0])
                                            ];
                                    }), param);
                      })),
                /* partnerPolicy */state[/* partnerPolicy */3]
              ];
    case 3 : 
        var match$2 = $$event[0];
        var supporterId$1 = match$2[/* supporterId */1];
        return /* record */[
                /* localUser */state[/* localUser */0],
                /* partners */state[/* partners */1],
                /* prospects */Belt_Map.update(state[/* prospects */2], match$2[/* processId */0], (function (param) {
                        return Utils.mapOption((function (prospect) {
                                      return /* record */[
                                              /* processId */prospect[/* processId */0],
                                              /* userId */prospect[/* userId */1],
                                              /* processType */prospect[/* processType */2],
                                              /* processStatus */prospect[/* processStatus */3],
                                              /* voters */Belt_List.mapU(prospect[/* voters */4], (function (param) {
                                                      var userId = param[/* userId */0];
                                                      var match = PrimitiveTypes.UserId[/* eq */5](userId, supporterId$1);
                                                      if (match) {
                                                        return /* record */[
                                                                /* userId */userId,
                                                                /* voteStatus : Endorsed */1
                                                              ];
                                                      } else {
                                                        return /* record */[
                                                                /* userId */userId,
                                                                /* voteStatus */param[/* voteStatus */1]
                                                              ];
                                                      }
                                                    })),
                                              /* canEndorse */prospect[/* canEndorse */5] && PrimitiveTypes.UserId[/* neq */6](supporterId$1, state[/* localUser */0]),
                                              /* canReject */prospect[/* canReject */6] && PrimitiveTypes.UserId[/* neq */6](supporterId$1, state[/* localUser */0])
                                            ];
                                    }), param);
                      })),
                /* partnerPolicy */state[/* partnerPolicy */3]
              ];
    case 4 : 
        var match$3 = $$event[0];
        var data = match$3[/* data */3];
        return /* record */[
                /* localUser */state[/* localUser */0],
                /* partners : :: */[
                  /* record */[
                    /* userId */data[/* id */1],
                    /* name : None */0,
                    /* canProposeRemoval */PrimitiveTypes.UserId[/* neq */6](data[/* id */1], state[/* localUser */0])
                  ],
                  state[/* partners */1]
                ],
                /* prospects */Belt_Map.update(state[/* prospects */2], match$3[/* processId */0], (function (param) {
                        return Utils.mapOption((function (prospect) {
                                      return /* record */[
                                              /* processId */prospect[/* processId */0],
                                              /* userId */prospect[/* userId */1],
                                              /* processType */prospect[/* processType */2],
                                              /* processStatus : Completed */0,
                                              /* voters */prospect[/* voters */4],
                                              /* canEndorse */prospect[/* canEndorse */5],
                                              /* canReject */prospect[/* canReject */6]
                                            ];
                                    }), param);
                      })),
                /* partnerPolicy */state[/* partnerPolicy */3]
              ];
    case 5 : 
        var match$4 = $$event[0];
        var data$1 = match$4[/* data */6];
        var supporterId$2 = match$4[/* supporterId */4];
        var eligibleWhenProposing$1 = match$4[/* eligibleWhenProposing */3];
        var processId$1 = match$4[/* processId */0];
        return /* record */[
                /* localUser */state[/* localUser */0],
                /* partners */Belt_List.map(state[/* partners */1], (function (p) {
                        var match = PrimitiveTypes.UserId[/* eq */5](p[/* userId */0], data$1[/* id */0]);
                        if (match) {
                          return /* record */[
                                  /* userId */p[/* userId */0],
                                  /* name */p[/* name */1],
                                  /* canProposeRemoval */false
                                ];
                        } else {
                          return p;
                        }
                      })),
                /* prospects */Belt_Map.set(state[/* prospects */2], processId$1, /* record */[
                      /* processId */processId$1,
                      /* userId */data$1[/* id */0],
                      /* processType : Removal */0,
                      /* processStatus : InProgress */1,
                      /* voters */Belt_List.mapU(Belt_Set.toList(eligibleWhenProposing$1), (function (userId) {
                              var match = PrimitiveTypes.UserId[/* eq */5](supporterId$2, userId);
                              return /* record */[
                                      /* userId */userId,
                                      /* voteStatus */match ? /* Endorsed */1 : /* Pending */0
                                    ];
                            })),
                      /* canEndorse */PrimitiveTypes.UserId[/* neq */6](supporterId$2, state[/* localUser */0]) && Belt_Set.has(eligibleWhenProposing$1, state[/* localUser */0]),
                      /* canReject */PrimitiveTypes.UserId[/* neq */6](supporterId$2, state[/* localUser */0]) && Belt_Set.has(eligibleWhenProposing$1, state[/* localUser */0])
                    ]),
                /* partnerPolicy */state[/* partnerPolicy */3]
              ];
    case 6 : 
        var match$5 = $$event[0];
        var rejectorId$1 = match$5[/* rejectorId */1];
        return /* record */[
                /* localUser */state[/* localUser */0],
                /* partners */state[/* partners */1],
                /* prospects */Belt_Map.update(state[/* prospects */2], match$5[/* processId */0], (function (param) {
                        return Utils.mapOption((function (prospect) {
                                      return /* record */[
                                              /* processId */prospect[/* processId */0],
                                              /* userId */prospect[/* userId */1],
                                              /* processType */prospect[/* processType */2],
                                              /* processStatus */prospect[/* processStatus */3],
                                              /* voters */Belt_List.mapU(prospect[/* voters */4], (function (param) {
                                                      var userId = param[/* userId */0];
                                                      var match = PrimitiveTypes.UserId[/* eq */5](userId, rejectorId$1);
                                                      if (match) {
                                                        return /* record */[
                                                                /* userId */userId,
                                                                /* voteStatus : Rejected */2
                                                              ];
                                                      } else {
                                                        return /* record */[
                                                                /* userId */userId,
                                                                /* voteStatus */param[/* voteStatus */1]
                                                              ];
                                                      }
                                                    })),
                                              /* canEndorse */prospect[/* canEndorse */5] && PrimitiveTypes.UserId[/* neq */6](rejectorId$1, state[/* localUser */0]),
                                              /* canReject */prospect[/* canReject */6] && PrimitiveTypes.UserId[/* neq */6](rejectorId$1, state[/* localUser */0])
                                            ];
                                    }), param);
                      })),
                /* partnerPolicy */state[/* partnerPolicy */3]
              ];
    case 7 : 
        var match$6 = $$event[0];
        var supporterId$3 = match$6[/* supporterId */1];
        return /* record */[
                /* localUser */state[/* localUser */0],
                /* partners */state[/* partners */1],
                /* prospects */Belt_Map.update(state[/* prospects */2], match$6[/* processId */0], (function (param) {
                        return Utils.mapOption((function (prospect) {
                                      return /* record */[
                                              /* processId */prospect[/* processId */0],
                                              /* userId */prospect[/* userId */1],
                                              /* processType */prospect[/* processType */2],
                                              /* processStatus */prospect[/* processStatus */3],
                                              /* voters */Belt_List.mapU(prospect[/* voters */4], (function (param) {
                                                      var userId = param[/* userId */0];
                                                      var match = PrimitiveTypes.UserId[/* eq */5](userId, supporterId$3);
                                                      if (match) {
                                                        return /* record */[
                                                                /* userId */userId,
                                                                /* voteStatus : Endorsed */1
                                                              ];
                                                      } else {
                                                        return /* record */[
                                                                /* userId */userId,
                                                                /* voteStatus */param[/* voteStatus */1]
                                                              ];
                                                      }
                                                    })),
                                              /* canEndorse */prospect[/* canEndorse */5] && PrimitiveTypes.UserId[/* neq */6](supporterId$3, state[/* localUser */0]),
                                              /* canReject */prospect[/* canReject */6] && PrimitiveTypes.UserId[/* neq */6](supporterId$3, state[/* localUser */0])
                                            ];
                                    }), param);
                      })),
                /* partnerPolicy */state[/* partnerPolicy */3]
              ];
    case 8 : 
        var match$7 = $$event[0];
        var id = match$7[/* data */3][/* id */0];
        return /* record */[
                /* localUser */state[/* localUser */0],
                /* partners */Belt_List.keep(state[/* partners */1], (function (p) {
                        return PrimitiveTypes.UserId[/* neq */6](p[/* userId */0], id);
                      })),
                /* prospects */Belt_Map.update(state[/* prospects */2], match$7[/* processId */0], (function (param) {
                        return Utils.mapOption((function (prospect) {
                                      return /* record */[
                                              /* processId */prospect[/* processId */0],
                                              /* userId */prospect[/* userId */1],
                                              /* processType */prospect[/* processType */2],
                                              /* processStatus : Completed */0,
                                              /* voters */prospect[/* voters */4],
                                              /* canEndorse */prospect[/* canEndorse */5],
                                              /* canReject */prospect[/* canReject */6]
                                            ];
                                    }), param);
                      })),
                /* partnerPolicy */state[/* partnerPolicy */3]
              ];
    default:
      return state;
  }
}

function isPartner(id, param) {
  return Belt_List.some(param[/* partners */1], (function (param) {
                return PrimitiveTypes.UserId[/* eq */5](param[/* userId */0], id);
              }));
}

exports.getProspect = getProspect;
exports.prospectsPendingApproval = prospectsPendingApproval;
exports.make = make;
exports.apply = apply;
exports.isPartner = isPartner;
/* Utils Not a pure module */
