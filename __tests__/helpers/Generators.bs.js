// Generated by BUCKLESCRIPT VERSION 3.0.0, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Event = require("../../src/application/events/Event.bs.js");
var Utils = require("../../src/utils/Utils.bs.js");
var Js_exn = require("bs-platform/lib/js/js_exn.js");
var Policy = require("../../src/application/Policy.bs.js");
var $$String = require("bs-platform/lib/js/string.js");
var Crypto = require("crypto");
var Network = require("../../src/application/wallet/Network.bs.js");
var EventLog = require("../../src/application/events/EventLog.bs.js");
var UserInfo = require("../../src/application/UserInfo.bs.js");
var WalletTypes = require("../../src/application/wallet/WalletTypes.bs.js");
var BitcoinjsLib = require("bitcoinjs-lib");
var PrimitiveTypes = require("../../src/application/PrimitiveTypes.bs.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

function userSession(id) {
  var appPrivateKey = Utils.bufToHex(Crypto.randomBytes(32));
  var issuerKeyPair = Utils.keyPairFromPrivateKey(Network.bitcoinNetwork(/* Regtest */0), appPrivateKey);
  var appPubKey = Utils.publicKeyFromKeyPair(issuerKeyPair);
  return /* record */[
          /* userId */id,
          /* appPrivateKey */appPrivateKey,
          /* issuerKeyPair */issuerKeyPair,
          /* storagePrefix */UserInfo.storagePrefix(Utils.publicKeyFromKeyPair(issuerKeyPair)),
          /* masterKeyChain */new BitcoinjsLib.HDNode(issuerKeyPair, Utils.bufFromHex($$String.sub(appPubKey, 0, 64))),
          /* network : Regtest */0
        ];
}

function twoUserSessions() {
  return /* tuple */[
          userSession(PrimitiveTypes.UserId[/* fromString */1]("user1")),
          userSession(PrimitiveTypes.UserId[/* fromString */1]("user2"))
        ];
}

function threeUserSessions() {
  return /* tuple */[
          userSession(PrimitiveTypes.UserId[/* fromString */1]("user1")),
          userSession(PrimitiveTypes.UserId[/* fromString */1]("user2")),
          userSession(PrimitiveTypes.UserId[/* fromString */1]("user3"))
        ];
}

function createVenture(session) {
  return Event.VentureCreated[/* make */0](PrimitiveTypes.UserId[/* toString */0](session[/* userId */0]) + "-testventure", session[/* userId */0], Utils.publicKeyFromKeyPair(session[/* issuerKeyPair */2]), Policy.absolute, session[/* network */5]);
}

function partnerProposed(supporterSession, prospectSession) {
  return Event.getPartnerProposedExn(Event.makePartnerProposed(supporterSession[/* userId */0], prospectSession[/* userId */0], Utils.publicKeyFromKeyPair(prospectSession[/* issuerKeyPair */2]), Policy.absolute));
}

function partnerEndorsed(supporter, param) {
  return Event.getPartnerEndorsedExn(Event.makePartnerEndorsed(param[/* processId */0], supporter[/* userId */0]));
}

var partnerAccepted = Event.Partner[/* Accepted */4][/* fromProposal */0];

function partnerRemovalProposed(supporterSession, toBeRemoved) {
  return Event.getPartnerRemovalProposedExn(Event.makePartnerRemovalProposed(supporterSession[/* userId */0], toBeRemoved[/* userId */0], Policy.absoluteMinusOne));
}

function partnerRemovalEndorsed(supporter, param) {
  return Event.getPartnerRemovalEndorsedExn(Event.makePartnerRemovalEndorsed(param[/* processId */0], supporter[/* userId */0]));
}

var partnerRemovalAccepted = Event.Partner[/* Removal */5][/* Accepted */4][/* fromProposal */0];

function accountCreationProposed(param) {
  return Event.getAccountCreationProposedExn(Event.makeAccountCreationProposed(param[/* userId */0], "test", WalletTypes.AccountIndex[/* default */8], Policy.absolute));
}

var accountCreationAccepted = Event.AccountCreation[/* Accepted */4][/* fromProposal */0];

function custodianProposed(param, partnerProposal) {
  return Event.getCustodianProposedExn(Event.makeCustodianProposed(partnerProposal[/* processId */0], param[/* userId */0], partnerProposal[/* data */4][/* id */0], WalletTypes.AccountIndex[/* default */8], Policy.absolute));
}

function custodianEndorsed(supporter, param) {
  return Event.getCustodianEndorsedExn(Event.makeCustodianEndorsed(param[/* processId */0], supporter[/* userId */0]));
}

var custodianAccepted = Event.Custodian[/* Accepted */4][/* fromProposal */0];

var Event$1 = /* module */[
  /* createVenture */createVenture,
  /* partnerProposed */partnerProposed,
  /* partnerEndorsed */partnerEndorsed,
  /* partnerAccepted */partnerAccepted,
  /* partnerRemovalProposed */partnerRemovalProposed,
  /* partnerRemovalEndorsed */partnerRemovalEndorsed,
  /* partnerRemovalAccepted */partnerRemovalAccepted,
  /* accountCreationProposed */accountCreationProposed,
  /* accountCreationAccepted */accountCreationAccepted,
  /* custodianProposed */custodianProposed,
  /* custodianEndorsed */custodianEndorsed,
  /* custodianAccepted */custodianAccepted
];

function systemIssuer(param) {
  return param[/* systemIssuer */0];
}

function lastItem(param) {
  return param[/* lastItem */1];
}

function lastEvent(param) {
  return param[/* lastItem */1][/* event */0];
}

function eventLog(param) {
  return param[/* log */2];
}

function appendEvent(issuer, $$event, l) {
  var match = Curry._3(EventLog.append, issuer, $$event, l[/* log */2]);
  return /* record */[
          /* systemIssuer */l[/* systemIssuer */0],
          /* lastItem */match[0],
          /* log */match[1]
        ];
}

function appendSystemEvent($$event, log) {
  return appendEvent(log[/* systemIssuer */0], $$event, log);
}

function createVenture$1(session) {
  var ventureCreated = createVenture(session);
  var match = Curry._3(EventLog.append, session[/* issuerKeyPair */2], /* VentureCreated */Block.__(0, [ventureCreated]), Curry._1(EventLog.make, /* () */0));
  return /* record */[
          /* systemIssuer */ventureCreated[/* systemIssuer */5],
          /* lastItem */match[0],
          /* log */match[1]
        ];
}

function withPartnerProposed(supporter, prospect) {
  var partial_arg = /* PartnerProposed */Block.__(1, [partnerProposed(supporter, prospect)]);
  var partial_arg$1 = supporter[/* issuerKeyPair */2];
  return (function (param) {
      return appendEvent(partial_arg$1, partial_arg, param);
    });
}

function withPartnerEndorsed(supporter, proposal) {
  var partial_arg = /* PartnerEndorsed */Block.__(2, [partnerEndorsed(supporter, proposal)]);
  var partial_arg$1 = supporter[/* issuerKeyPair */2];
  return (function (param) {
      return appendEvent(partial_arg$1, partial_arg, param);
    });
}

function withPartnerAccepted(proposal) {
  var partial_arg = /* PartnerAccepted */Block.__(3, [Curry._1(partnerAccepted, proposal)]);
  return (function (param) {
      return appendSystemEvent(partial_arg, param);
    });
}

function withPartner(user, supporters, log) {
  if (supporters) {
    var log$1 = withPartnerProposed(supporters[0], user)(log);
    var proposal = Event.getPartnerProposedExn(lastEvent(log$1));
    return withPartnerAccepted(proposal)(List.fold_left((function (log, supporter) {
                      return withPartnerEndorsed(supporter, proposal)(log);
                    }), log$1, supporters[1]));
  } else {
    return Js_exn.raiseError("withPartner");
  }
}

function withFirstPartner(user) {
  var partial_arg = /* :: */[
    user,
    /* [] */0
  ];
  return (function (param) {
      return withPartner(user, partial_arg, param);
    });
}

function withPartnerRemovalProposed(supporter, toBeRemoved) {
  var partial_arg = /* PartnerRemovalProposed */Block.__(4, [partnerRemovalProposed(supporter, toBeRemoved)]);
  var partial_arg$1 = supporter[/* issuerKeyPair */2];
  return (function (param) {
      return appendEvent(partial_arg$1, partial_arg, param);
    });
}

function withPartnerRemovalEndorsed(supporter, proposal) {
  var partial_arg = /* PartnerRemovalEndorsed */Block.__(5, [partnerRemovalEndorsed(supporter, proposal)]);
  var partial_arg$1 = supporter[/* issuerKeyPair */2];
  return (function (param) {
      return appendEvent(partial_arg$1, partial_arg, param);
    });
}

function withPartnerRemovalAccepted(proposal) {
  var partial_arg = /* PartnerRemovalAccepted */Block.__(6, [Curry._1(partnerRemovalAccepted, proposal)]);
  return (function (param) {
      return appendSystemEvent(partial_arg, param);
    });
}

function withPartnerRemoved(user, supporters, log) {
  if (supporters) {
    var log$1 = withPartnerRemovalProposed(supporters[0], user)(log);
    var proposal = Event.getPartnerRemovalProposedExn(lastEvent(log$1));
    return withPartnerRemovalAccepted(proposal)(List.fold_left((function (log, supporter) {
                      return withPartnerRemovalEndorsed(supporter, proposal)(log);
                    }), log$1, supporters[1]));
  } else {
    return Js_exn.raiseError("withPartner");
  }
}

function withAccountCreationProposed(supporter) {
  var partial_arg = /* AccountCreationProposed */Block.__(7, [accountCreationProposed(supporter)]);
  var partial_arg$1 = supporter[/* issuerKeyPair */2];
  return (function (param) {
      return appendEvent(partial_arg$1, partial_arg, param);
    });
}

function withAccountCreationAccepted(proposal) {
  var partial_arg = /* AccountCreationAccepted */Block.__(9, [Curry._1(accountCreationAccepted, proposal)]);
  return (function (param) {
      return appendSystemEvent(partial_arg, param);
    });
}

function withCustodianProposed(supporter, custodian, l) {
  var partnerProposed = Curry._3(EventLog.reduce, (function (partnerProposal, param) {
          var $$event = param[/* event */0];
          if (partnerProposal) {
            return /* Some */[partnerProposal[0]];
          } else if ($$event.tag === 1) {
            var proposal = $$event[0];
            if (PrimitiveTypes.UserId[/* eq */5](proposal[/* data */4][/* id */0], custodian[/* userId */0])) {
              return /* Some */[proposal];
            } else {
              return partnerProposal;
            }
          } else {
            return partnerProposal;
          }
        }), /* None */0, l[/* log */2]);
  if (partnerProposed) {
    return appendEvent(supporter[/* issuerKeyPair */2], /* CustodianProposed */Block.__(10, [custodianProposed(supporter, partnerProposed[0])]), l);
  } else {
    throw Caml_builtin_exceptions.not_found;
  }
}

function withCustodianEndorsed(supporter, proposal) {
  var partial_arg = /* CustodianEndorsed */Block.__(11, [custodianEndorsed(supporter, proposal)]);
  var partial_arg$1 = supporter[/* issuerKeyPair */2];
  return (function (param) {
      return appendEvent(partial_arg$1, partial_arg, param);
    });
}

function withCustodianAccepted(proposal) {
  var partial_arg = /* CustodianAccepted */Block.__(12, [Curry._1(custodianAccepted, proposal)]);
  return (function (param) {
      return appendSystemEvent(partial_arg, param);
    });
}

var Log = /* module */[
  /* systemIssuer */systemIssuer,
  /* lastItem */lastItem,
  /* lastEvent */lastEvent,
  /* eventLog */eventLog,
  /* appendEvent */appendEvent,
  /* appendSystemEvent */appendSystemEvent,
  /* createVenture */createVenture$1,
  /* withPartnerProposed */withPartnerProposed,
  /* withPartnerEndorsed */withPartnerEndorsed,
  /* withPartnerAccepted */withPartnerAccepted,
  /* withPartner */withPartner,
  /* withFirstPartner */withFirstPartner,
  /* withPartnerRemovalProposed */withPartnerRemovalProposed,
  /* withPartnerRemovalEndorsed */withPartnerRemovalEndorsed,
  /* withPartnerRemovalAccepted */withPartnerRemovalAccepted,
  /* withPartnerRemoved */withPartnerRemoved,
  /* withAccountCreationProposed */withAccountCreationProposed,
  /* withAccountCreationAccepted */withAccountCreationAccepted,
  /* withCustodianProposed */withCustodianProposed,
  /* withCustodianEndorsed */withCustodianEndorsed,
  /* withCustodianAccepted */withCustodianAccepted
];

var AppEvent = 0;

exports.AppEvent = AppEvent;
exports.userSession = userSession;
exports.twoUserSessions = twoUserSessions;
exports.threeUserSessions = threeUserSessions;
exports.Event = Event$1;
exports.Log = Log;
/* Event Not a pure module */
