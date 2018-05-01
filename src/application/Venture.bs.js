// Generated by BUCKLESCRIPT VERSION 3.0.0, PLEASE EDIT WITH CARE
'use strict';

var Json = require("bs-json/src/Json.js");
var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Event = require("./events/Event.bs.js");
var Utils = require("../utils/Utils.bs.js");
var Policy = require("./Policy.bs.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var EventLog = require("./events/EventLog.bs.js");
var UserInfo = require("./UserInfo.bs.js");
var Watchers = require("./watcher/Watchers.bs.js");
var Js_option = require("bs-platform/lib/js/js_option.js");
var Blockstack = require("../ffi/Blockstack.bs.js");
var Blockstack$1 = require("blockstack");
var WalletTypes = require("./wallet/WalletTypes.bs.js");
var PrimitiveTypes = require("./PrimitiveTypes.bs.js");
var Venture__Index = require("./Venture__Index.bs.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");
var Venture__Wallet = require("./Venture__Wallet.bs.js");
var Venture__Validation = require("./Venture__Validation.bs.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");
var EncryptionJs = require("blockstack/lib/encryption.js");

function logMessage(msg) {
  console.log("[Venture] - " + msg);
  return /* () */0;
}

var InvalidEvent = Caml_exceptions.create("Venture.InvalidEvent");

var CouldNotLoadVenture = Caml_exceptions.create("Venture.CouldNotLoadVenture");

function balance(param) {
  return Venture__Wallet.balance(WalletTypes.AccountIndex[/* default */8], param[/* wallet */4]);
}

function getExposedAddresses(param) {
  return Venture__Wallet.getExposedAddresses(/* None */0, param[/* wallet */4]);
}

function getKnownTransactionIds(param) {
  return Venture__Wallet.getKnownTransactionIds(param[/* wallet */4]);
}

function make(session, id, listenerState, listener) {
  return /* record */[
          /* session */session,
          /* id */id,
          /* log */Curry._1(EventLog.make, /* () */0),
          /* state */Venture__Validation.makeState(/* () */0),
          /* wallet */Venture__Wallet.make(/* () */0),
          /* listener */listener,
          /* listenerState */listenerState,
          /* watchers : [] */0
        ];
}

function applyInternal($staropt$star, issuer, $$event, oldLog, param) {
  var match = param[2];
  var listener = match[1];
  var listenerState = match[0];
  var wallet = param[1];
  var state = param[0];
  var syncing = $staropt$star ? $staropt$star[0] : false;
  var match$1 = Curry._3(EventLog.append, issuer, $$event, oldLog);
  var item = match$1[0];
  var result = Venture__Validation.validate(state, item);
  var exit = 0;
  if (typeof result === "number") {
    if (result !== 1) {
      if (result !== 0) {
        exit = 1;
      } else {
        logMessage("Appended event to log:");
        logMessage(Json.stringify(Event.encode($$event)));
        var state$1 = Venture__Validation.apply($$event, state);
        var wallet$1 = Venture__Wallet.apply($$event, wallet);
        var listenerState$1 = Curry._2(listener, $$event, listenerState);
        return /* tuple */[
                /* Some */[item],
                match$1[1],
                /* tuple */[
                  state$1,
                  wallet$1,
                  /* tuple */[
                    listenerState$1,
                    listener
                  ]
                ]
              ];
      }
    } else {
      logMessage("Ignoring event:");
      logMessage(Json.stringify(Event.encode($$event)));
      return /* tuple */[
              /* None */0,
              oldLog,
              /* tuple */[
                state,
                wallet,
                /* tuple */[
                  listenerState,
                  listener
                ]
              ]
            ];
    }
  } else {
    exit = 1;
  }
  if (exit === 1) {
    logMessage("Event was rejected because of:");
    logMessage(Venture__Validation.resultToString(result));
    if (syncing) {
      return /* tuple */[
              /* None */0,
              oldLog,
              /* tuple */[
                state,
                wallet,
                /* tuple */[
                  listenerState,
                  listener
                ]
              ]
            ];
    } else {
      throw [
            InvalidEvent,
            result
          ];
    }
  }
  
}

function apply($staropt$star, $$event, param) {
  var state = param[/* state */3];
  var id = param[/* id */1];
  var session = param[/* session */0];
  var systemEvent = $staropt$star ? $staropt$star[0] : false;
  var match = applyInternal(/* None */0, systemEvent ? state[/* systemIssuer */1] : session[/* issuerKeyPair */2], $$event, param[/* log */2], /* tuple */[
        state,
        param[/* wallet */4],
        /* tuple */[
          param[/* listenerState */6],
          param[/* listener */5]
        ]
      ]);
  var match$1 = match[2];
  var match$2 = match$1[2];
  return Watchers.applyAndProcessPending(session, match[0], match[1], (function (eta) {
                  return (function (param, param$1, param$2) {
                      return applyInternal(/* None */0, eta, param, param$1, param$2);
                    });
                }), /* tuple */[
                match$1[0],
                match$1[1],
                /* tuple */[
                  match$2[0],
                  match$2[1]
                ]
              ], param[/* watchers */7]).then((function (param) {
                var match = param[1];
                var match$1 = match[2];
                return Promise.resolve(/* record */[
                            /* session */session,
                            /* id */id,
                            /* log */param[0],
                            /* state */match[0],
                            /* wallet */match[1],
                            /* listener */match$1[1],
                            /* listenerState */match$1[0],
                            /* watchers */param[2]
                          ]);
              }));
}

function reconstruct(session, listenerState, listener, log) {
  var match = make(session, PrimitiveTypes.VentureId[/* make */7](/* () */0), listenerState, listener);
  var match$1 = Curry._3(EventLog.reduce, (function (param, item) {
          var $$event = item[/* event */0];
          var match = param[3];
          var listener = match[1];
          var tmp;
          tmp = $$event.tag ? param[0] : $$event[0][/* ventureId */0];
          return /* tuple */[
                  tmp,
                  Venture__Validation.apply($$event, param[1]),
                  Venture__Wallet.apply($$event, param[2]),
                  /* tuple */[
                    Curry._2(listener, $$event, match[0]),
                    listener
                  ],
                  Watchers.apply(/* Some */[true], session, /* Some */[item], log, param[4])
                ];
        }), /* tuple */[
        PrimitiveTypes.VentureId[/* make */7](/* () */0),
        match[/* state */3],
        match[/* wallet */4],
        /* tuple */[
          listenerState,
          listener
        ],
        /* [] */0
      ], log);
  var match$2 = match$1[3];
  var id = match$1[0];
  return Watchers.processPending(session, log, (function (eta) {
                  return (function (param, param$1, param$2) {
                      return applyInternal(/* None */0, eta, param, param$1, param$2);
                    });
                }), /* tuple */[
                match$1[1],
                match$1[2],
                /* tuple */[
                  match$2[0],
                  match$2[1]
                ]
              ], match$1[4]).then((function (param) {
                var match = param[1];
                var match$1 = match[2];
                return Promise.resolve(/* record */[
                            /* session */session,
                            /* id */id,
                            /* log */param[0],
                            /* state */match[0],
                            /* wallet */match[1],
                            /* listener */match$1[1],
                            /* listenerState */match$1[0],
                            /* watchers */param[2]
                          ]);
              }));
}

function persist(venture) {
  var log = venture[/* log */2];
  var id = venture[/* id */1];
  var logString = Json.stringify(Curry._1(EventLog.encode, log));
  var summaryString = Json.stringify(Curry._1(EventLog.encodeSummary, Curry._1(EventLog.getSummary, log)));
  var returnPromise = Blockstack$1.putFile(PrimitiveTypes.VentureId[/* toString */0](id) + "/log.json", logString).then((function () {
          return Promise.resolve(venture);
        }));
  List.fold_left((function (promise, param) {
          var prefix = param[1];
          var pubKey = param[0];
          return promise.then((function () {
                          return Promise.resolve((setTimeout((function () {
                                              Blockstack$1.putFile(PrimitiveTypes.VentureId[/* toString */0](id) + ("/" + (prefix + "/log.json")), Json.stringify(EncryptionJs.encryptECIES(pubKey, logString)), ( {"encrypt": false} ));
                                              return /* () */0;
                                            }), 1), /* () */0));
                        })).then((function () {
                        return Promise.resolve((setTimeout((function () {
                                            Blockstack$1.putFile(PrimitiveTypes.VentureId[/* toString */0](id) + ("/" + (prefix + "/summary.json")), summaryString, ( {"encrypt": false} ));
                                            return /* () */0;
                                          }), 1), /* () */0));
                      }));
        }), Promise.resolve(/* () */0), venture[/* state */3][/* partnerStoragePrefixes */5]);
  return returnPromise;
}

function load(session, ventureId, listenerState, listener) {
  logMessage("Loading venture '" + (PrimitiveTypes.VentureId[/* toString */0](ventureId) + "'"));
  return Blockstack$1.getFile(PrimitiveTypes.VentureId[/* toString */0](ventureId) + "/log.json").then((function (nullLog) {
                  if (nullLog == null) {
                    throw CouldNotLoadVenture;
                  } else {
                    return reconstruct(session, listenerState, listener, Curry._1(EventLog.decode, Json.parseOrRaise(nullLog)));
                  }
                })).then(persist);
}

function join(session, userId, ventureId, listenerState, listener) {
  return Blockstack.getFileFromUserAndDecrypt(PrimitiveTypes.VentureId[/* toString */0](ventureId) + ("/" + (session[/* storagePrefix */3] + "/log.json")), PrimitiveTypes.UserId[/* toString */0](userId)).catch((function () {
                      throw Caml_builtin_exceptions.not_found;
                    })).then((function (nullFile) {
                    if (nullFile == null) {
                      throw Caml_builtin_exceptions.not_found;
                    } else {
                      return reconstruct(session, listenerState, listener, Curry._1(EventLog.decode, Json.parseOrRaise(nullFile)));
                    }
                  })).then(persist).then((function (venture) {
                return Venture__Index.add(venture[/* id */1], venture[/* state */3][/* ventureName */0]).then((function (index) {
                              return Promise.resolve(/* tuple */[
                                          index,
                                          venture
                                        ]);
                            }));
              }));
}

function getId(param) {
  return param[/* id */1];
}

function getSummary(param) {
  return Curry._1(EventLog.getSummary, param[/* log */2]);
}

function getListenerState(param) {
  return param[/* listenerState */6];
}

function getPartnerHistoryUrls(param) {
  var id = param[/* id */1];
  var session = param[/* session */0];
  return Promise.all($$Array.of_list(List.map((function (partnerId) {
                        return Blockstack$1.getUserAppFileUrl(PrimitiveTypes.VentureId[/* toString */0](id) + ("/" + session[/* storagePrefix */3]), PrimitiveTypes.UserId[/* toString */0](partnerId), window.location.origin);
                      }), List.filter((function (partnerId) {
                              return PrimitiveTypes.UserId[/* neq */6](partnerId, session[/* userId */0]);
                            }))(param[/* state */3][/* partnerIds */4]))));
}

function exec(otherLogs, venture) {
  var session = venture[/* session */0];
  var otherLogs$1 = List.map((function (encryptedLog) {
          return Curry._1(EventLog.decode, Json.parseOrRaise(EncryptionJs.decryptECIES(session[/* appPrivateKey */1], encryptedLog)));
        }), otherLogs);
  var newItems = Curry._2(EventLog.findNewItems, otherLogs$1, venture[/* log */2]);
  var match = List.fold_left((function (param, item) {
          var $$event = item[/* event */0];
          var error = param[1];
          var venture = param[0];
          var state = venture[/* state */3];
          if (Js_option.isSome(error)) {
            return /* tuple */[
                    venture,
                    error
                  ];
          } else {
            var conflict = Venture__Validation.validate(state, item);
            if (typeof conflict === "number") {
              var exit = 0;
              switch (conflict) {
                case 0 : 
                    logMessage("Appending synced event to log:");
                    logMessage(Json.stringify(Event.encode($$event)));
                    var log = Curry._2(EventLog.appendItem, item, venture[/* log */2]);
                    var state$1 = Venture__Validation.apply($$event, state);
                    var wallet = Venture__Wallet.apply($$event, venture[/* wallet */4]);
                    var listenerState = Curry._2(venture[/* listener */5], $$event, venture[/* listenerState */6]);
                    var watchers = Watchers.apply(/* None */0, session, /* Some */[item], log, venture[/* watchers */7]);
                    return /* tuple */[
                            /* record */[
                              /* session */venture[/* session */0],
                              /* id */venture[/* id */1],
                              /* log */log,
                              /* state */state$1,
                              /* wallet */wallet,
                              /* listener */venture[/* listener */5],
                              /* listenerState */listenerState,
                              /* watchers */watchers
                            ],
                            /* None */0
                          ];
                case 1 : 
                    return /* tuple */[
                            venture,
                            /* None */0
                          ];
                case 2 : 
                    logMessage("Invalid issuer detected");
                    return /* tuple */[
                            venture,
                            /* None */0
                          ];
                case 3 : 
                    logMessage("Unknown ProcessId detected");
                    return /* tuple */[
                            venture,
                            /* None */0
                          ];
                case 4 : 
                case 5 : 
                    exit = 1;
                    break;
                case 6 : 
                    logMessage("Dependency Not Met detected");
                    return /* tuple */[
                            venture,
                            /* None */0
                          ];
                
              }
              if (exit === 1) {
                return /* tuple */[
                        venture,
                        /* Some */[/* Error */Block.__(1, [
                              venture,
                              item,
                              conflict
                            ])]
                      ];
              }
              
            } else {
              logMessage("Bad data in event detected: " + conflict[0]);
              return /* tuple */[
                      venture,
                      /* None */0
                    ];
            }
          }
        }), /* tuple */[
        venture,
        /* None */0
      ], newItems);
  var error = match[1];
  var match$1 = match[0];
  var partial_arg = /* Some */[true];
  return Watchers.processPending(session, match$1[/* log */2], (function (param, param$1, param$2, param$3) {
                    return applyInternal(partial_arg, param, param$1, param$2, param$3);
                  }), /* tuple */[
                  match$1[/* state */3],
                  match$1[/* wallet */4],
                  /* tuple */[
                    match$1[/* listenerState */6],
                    match$1[/* listener */5]
                  ]
                ], match$1[/* watchers */7]).then((function (param) {
                  var match = param[1];
                  var match$1 = match[2];
                  return persist(/* record */[
                              /* session */venture[/* session */0],
                              /* id */venture[/* id */1],
                              /* log */param[0],
                              /* state */match[0],
                              /* wallet */match[1],
                              /* listener */match$1[1],
                              /* listenerState */match$1[0],
                              /* watchers */param[2]
                            ]);
                })).then((function (p) {
                return Promise.resolve(error ? error[0] : /* Ok */Block.__(0, [p]));
              }));
}

function exec$1(session, ventureName, listenerState, listener) {
  logMessage("Executing 'Create' command");
  var ventureCreated = Event.VentureCreated[/* make */0](ventureName, session[/* userId */0], Utils.publicKeyFromKeyPair(session[/* issuerKeyPair */2]), Policy.unanimous, session[/* network */5]);
  return Promise.all(/* tuple */[
              Venture__Index.add(ventureCreated[/* ventureId */0], ventureName),
              apply(/* None */0, /* VentureCreated */Block.__(0, [ventureCreated]), make(session, ventureCreated[/* ventureId */0], listenerState, listener)).then(persist)
            ]);
}

var Create = /* module */[/* exec */exec$1];

function exec$2(newTransactions, venture) {
  var wallet = venture[/* wallet */4];
  var events = List.flatten(List.map((function (tx) {
              return Venture__Wallet.registerIncomeTransaction(tx, wallet);
            }), newTransactions));
  if (events) {
    return List.fold_left((function (p, $$event) {
                      return p.then((function (v) {
                                    return apply(/* Some */[true], $$event, v);
                                  }));
                    }), Promise.resolve(venture), events).then(persist).then((function (venture) {
                  return Promise.resolve(/* Ok */[venture]);
                }));
  } else {
    return Promise.resolve(/* Ok */[venture]);
  }
}

var SynchronizeWallet = /* module */[/* exec */exec$2];

function exec$3(prospectId, venture) {
  var state = venture[/* state */3];
  var session = venture[/* session */0];
  logMessage("Executing 'ProposePartner' command");
  if (List.mem(prospectId, state[/* partnerIds */4])) {
    return Promise.resolve(/* PartnerAlreadyExists */0);
  } else {
    return UserInfo.Public[/* read */4](prospectId).then((function (param) {
                  if (param) {
                    var partnerProposal = Event.getPartnerProposedExn(Event.makePartnerProposed(session[/* userId */0], prospectId, param[0][/* appPubKey */0], List.assoc(Event.Partner[/* processName */1], state[/* policies */15])));
                    var custodianProposal = Event.getCustodianProposedExn(Event.makeCustodianProposed(partnerProposal[/* processId */0], session[/* userId */0], prospectId, WalletTypes.AccountIndex[/* default */8], List.assoc(Event.Custodian[/* processName */1], state[/* policies */15])));
                    var partial_arg = /* CustodianProposed */Block.__(10, [custodianProposal]);
                    return apply(/* None */0, /* PartnerProposed */Block.__(1, [partnerProposal]), venture).then((function (param) {
                                      return apply(/* None */0, partial_arg, param);
                                    })).then(persist).then((function (p) {
                                  return Promise.resolve(/* Ok */[p]);
                                }));
                  } else {
                    return Promise.resolve(/* NoUserInfo */1);
                  }
                }));
  }
}

var ProposePartner = /* module */[/* exec */exec$3];

function exec$4(processId, venture) {
  var state = venture[/* state */3];
  var session = venture[/* session */0];
  logMessage("Executing 'EndorsePartner' command");
  var match = List.assoc(processId, state[/* partnerData */7]);
  var partnerId = match[/* id */0];
  var match$1 = List.find((function (param) {
          return Caml_obj.caml_equal(param[1][/* partnerId */0], partnerId);
        }), state[/* custodianData */9]);
  var partial_arg = Event.makeCustodianEndorsed(match$1[0], session[/* userId */0]);
  return apply(/* None */0, Event.makePartnerEndorsed(processId, session[/* userId */0]), venture).then((function (param) {
                    return apply(/* None */0, partial_arg, param);
                  })).then(persist).then((function (p) {
                return Promise.resolve(/* Ok */[p]);
              }));
}

var EndorsePartner = /* module */[/* exec */exec$4];

function exec$5(partnerId, venture) {
  var state = venture[/* state */3];
  var session = venture[/* session */0];
  logMessage("Executing 'ProposePartnerRemoval' command");
  if (List.mem(partnerId, state[/* partnerIds */4]) === false) {
    return Promise.resolve(/* PartnerDoesNotExist */0);
  } else {
    var match = List.find((function (param) {
            return PrimitiveTypes.UserId[/* eq */5](partnerId, param[1][/* partnerId */0]);
          }), state[/* custodianData */9]);
    var partial_arg = Event.makePartnerRemovalProposed(session[/* userId */0], partnerId, List.assoc(Event.Partner[/* Removal */5][/* processName */1], state[/* policies */15]));
    return apply(/* None */0, Event.makeCustodianRemovalProposed(/* Some */[match[0]], session[/* userId */0], partnerId, WalletTypes.AccountIndex[/* default */8], List.assoc(Event.Custodian[/* Removal */5][/* processName */1], state[/* policies */15])), venture).then((function (param) {
                      return apply(/* None */0, partial_arg, param);
                    })).then(persist).then((function (p) {
                  return Promise.resolve(/* Ok */[p]);
                }));
  }
}

var ProposePartnerRemoval = /* module */[/* exec */exec$5];

function exec$6(processId, venture) {
  var state = venture[/* state */3];
  var session = venture[/* session */0];
  logMessage("Executing 'EndorsePartnerRemoval' command");
  var match = List.assoc(processId, state[/* partnerRemovalData */8]);
  var partnerId = match[/* id */0];
  var match$1 = List.find((function (param) {
          return Caml_obj.caml_equal(param[1][/* custodianId */0], partnerId);
        }), state[/* custodianRemovalData */10]);
  var partial_arg = Event.makePartnerRemovalEndorsed(processId, session[/* userId */0]);
  return apply(/* None */0, Event.makeCustodianRemovalEndorsed(match$1[0], session[/* userId */0]), venture).then((function (param) {
                    return apply(/* None */0, partial_arg, param);
                  })).then(persist).then((function (p) {
                return Promise.resolve(/* Ok */[p]);
              }));
}

var EndorsePartnerRemoval = /* module */[/* exec */exec$6];

function exec$7(accountIdx, venture) {
  logMessage("Executing 'GetIncomeAddress' command");
  var exposeEvent = Venture__Wallet.exposeNextIncomeAddress(accountIdx, venture[/* wallet */4]);
  return apply(/* Some */[true], /* IncomeAddressExposed */Block.__(25, [exposeEvent]), venture).then(persist).then((function (p) {
                return Promise.resolve(/* Ok */[
                            exposeEvent[/* address */1],
                            p
                          ]);
              }));
}

var ExposeIncomeAddress = /* module */[/* exec */exec$7];

function exec$8(accountIdx, destinations, fee, venture) {
  logMessage("Executing 'ProposePayout' command");
  return Venture__Wallet.preparePayoutTx(venture[/* session */0], accountIdx, destinations, fee, venture[/* wallet */4]).then((function (proposal) {
                    return apply(/* None */0, /* PayoutProposed */Block.__(16, [proposal]), venture);
                  })).then(persist).then((function (p) {
                return Promise.resolve(/* Ok */[p]);
              }));
}

var ProposePayout = /* module */[/* exec */exec$8];

function exec$9(processId, venture) {
  logMessage("Executing 'EndorsePayout' command");
  return apply(/* None */0, Event.makePayoutEndorsed(processId, venture[/* session */0][/* userId */0]), venture).then(persist).then((function (p) {
                return Promise.resolve(/* Ok */[p]);
              }));
}

var EndorsePayout = /* module */[/* exec */exec$9];

var Index = [Venture__Index.load];

var Validation = [Venture__Validation.resultToString];

var Wallet = [
  balance,
  getExposedAddresses,
  getKnownTransactionIds
];

var Cmd_001 = [exec];

var Cmd = [
  Create,
  Cmd_001,
  SynchronizeWallet,
  ProposePartner,
  EndorsePartner,
  ProposePartnerRemoval,
  EndorsePartnerRemoval,
  ExposeIncomeAddress,
  ProposePayout,
  EndorsePayout
];

exports.Index = Index;
exports.Validation = Validation;
exports.InvalidEvent = InvalidEvent;
exports.CouldNotLoadVenture = CouldNotLoadVenture;
exports.join = join;
exports.load = load;
exports.getId = getId;
exports.getSummary = getSummary;
exports.getListenerState = getListenerState;
exports.getPartnerHistoryUrls = getPartnerHistoryUrls;
exports.Wallet = Wallet;
exports.Cmd = Cmd;
/* Event Not a pure module */
