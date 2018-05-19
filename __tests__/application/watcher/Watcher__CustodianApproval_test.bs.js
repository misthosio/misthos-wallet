// Generated by BUCKLESCRIPT VERSION 3.0.0, PLEASE EDIT WITH CARE
'use strict';

var Event = require("../../../src/application/events/Event.bs.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Generators = require("../../helpers/Generators.bs.js");
var Caml_oo_curry = require("bs-platform/lib/js/caml_oo_curry.js");
var PrimitiveTypes = require("../../../src/application/PrimitiveTypes.bs.js");
var WatcherHelpers = require("../../helpers/WatcherHelpers.bs.js");
var Watcher__CustodianApproval = require("../../../src/application/watcher/Watcher__CustodianApproval.bs.js");

describe("With 1 partner and a proposal", (function () {
        var user1 = Generators.userSession(PrimitiveTypes.UserId[/* fromString */1]("user1"));
        var log = Generators.Log[/* withCustodianProposed */25](user1, user1, Generators.Log[/* withFirstPartner */17](user1)(Generators.Log[/* createVenture */11](user1)));
        var proposal = Event.getCustodianProposedExn(Generators.Log[/* lastEvent */5](log));
        var watcher = Watcher__CustodianApproval.make(proposal, Generators.Log[/* eventLog */6](log));
        return WatcherHelpers.testWatcherHasEventPending("CustodianAccepted", watcher, Generators.Log[/* systemIssuer */3](log), (function (param) {
                      if (param.tag === 16) {
                        return Caml_obj.caml_equal(param[0][/* data */3], proposal[/* data */6]);
                      } else {
                        return false;
                      }
                    }));
      }));

describe("Completes when the custodian is accepted", (function () {
        var user1 = Generators.userSession(PrimitiveTypes.UserId[/* fromString */1]("user1"));
        var log = Generators.Log[/* withCustodianProposed */25](user1, user1, Generators.Log[/* withFirstPartner */17](user1)(Generators.Log[/* createVenture */11](user1)));
        var proposal = Event.getCustodianProposedExn(Generators.Log[/* lastEvent */5](log));
        var watcher = Watcher__CustodianApproval.make(proposal, Generators.Log[/* eventLog */6](log));
        var log$1 = Generators.Log[/* withCustodianAccepted */27](proposal)(log);
        Caml_oo_curry.js2(710435299, 1, watcher, Generators.Log[/* lastItem */4](log$1));
        return WatcherHelpers.testWatcherHasCompleted(watcher);
      }));

describe("Completes when the partner is removed", (function () {
        var match = Generators.twoUserSessions(/* () */0);
        var user2 = match[1];
        var user1 = match[0];
        var log = Generators.Log[/* withCustodianProposed */25](user1, user2, Generators.Log[/* withPartner */16](user2, /* :: */[
                  user1,
                  /* [] */0
                ], Generators.Log[/* withFirstPartner */17](user1)(Generators.Log[/* createVenture */11](user1))));
        var proposal = Event.getCustodianProposedExn(Generators.Log[/* lastEvent */5](log));
        var log$1 = Generators.Log[/* withPartnerRemoved */21](user2, /* :: */[
              user1,
              /* [] */0
            ], log);
        return WatcherHelpers.testWatcherHasCompleted(Watcher__CustodianApproval.make(proposal, Generators.Log[/* eventLog */6](log$1)));
      }));

describe("With 2 users and a proposal", (function () {
        var match = Generators.twoUserSessions(/* () */0);
        var user2 = match[1];
        var user1 = match[0];
        var log = Generators.Log[/* withCustodianProposed */25](user1, user2, Generators.Log[/* withPartner */16](user2, /* :: */[
                  user1,
                  /* [] */0
                ], Generators.Log[/* withFirstPartner */17](user1)(Generators.Log[/* createVenture */11](user1))));
        var proposal = Event.getCustodianProposedExn(Generators.Log[/* lastEvent */5](log));
        return WatcherHelpers.testWatcherHasNoEventPending(Watcher__CustodianApproval.make(proposal, Generators.Log[/* eventLog */6](log)));
      }));

describe("With 2 users and a proposal and endorsement", (function () {
        var match = Generators.twoUserSessions(/* () */0);
        var user2 = match[1];
        var user1 = match[0];
        var log = Generators.Log[/* withCustodianProposed */25](user1, user2, Generators.Log[/* withPartner */16](user2, /* :: */[
                  user1,
                  /* [] */0
                ], Generators.Log[/* withFirstPartner */17](user1)(Generators.Log[/* createVenture */11](user1))));
        var proposal = Event.getCustodianProposedExn(Generators.Log[/* lastEvent */5](log));
        var log$1 = Generators.Log[/* withCustodianEndorsed */26](user2, proposal)(log);
        var watcher = Watcher__CustodianApproval.make(proposal, Generators.Log[/* eventLog */6](log$1));
        return WatcherHelpers.testWatcherHasEventPending("CustodianAccepted", watcher, Generators.Log[/* systemIssuer */3](log$1), (function (param) {
                      if (param.tag === 16) {
                        return Caml_obj.caml_equal(param[0][/* data */3], proposal[/* data */6]);
                      } else {
                        return false;
                      }
                    }));
      }));

describe("With 2 users and a removal and a proposal", (function () {
        var match = Generators.twoUserSessions(/* () */0);
        var user2 = match[1];
        var user1 = match[0];
        var log = Generators.Log[/* withCustodianProposed */25](user1, user1, Generators.Log[/* withPartnerRemoved */21](user2, /* :: */[
                  user1,
                  /* [] */0
                ], Generators.Log[/* withPartner */16](user2, /* :: */[
                      user1,
                      /* [] */0
                    ], Generators.Log[/* withFirstPartner */17](user1)(Generators.Log[/* createVenture */11](user1)))));
        var proposal = Event.getCustodianProposedExn(Generators.Log[/* lastEvent */5](log));
        var watcher = Watcher__CustodianApproval.make(proposal, Generators.Log[/* eventLog */6](log));
        return WatcherHelpers.testWatcherHasEventPending("CustodianAccepted", watcher, Generators.Log[/* systemIssuer */3](log), (function (param) {
                      if (param.tag === 16) {
                        return Caml_obj.caml_equal(param[0][/* data */3], proposal[/* data */6]);
                      } else {
                        return false;
                      }
                    }));
      }));

describe("With 2 users and a proposal and a removal", (function () {
        var match = Generators.twoUserSessions(/* () */0);
        var user2 = match[1];
        var user1 = match[0];
        var log = Generators.Log[/* withCustodianProposed */25](user1, user1, Generators.Log[/* withPartner */16](user2, /* :: */[
                  user1,
                  /* [] */0
                ], Generators.Log[/* withFirstPartner */17](user1)(Generators.Log[/* createVenture */11](user1))));
        var proposal = Event.getCustodianProposedExn(Generators.Log[/* lastEvent */5](log));
        var watcher = Watcher__CustodianApproval.make(proposal, Generators.Log[/* eventLog */6](log));
        var log$1 = Generators.Log[/* withPartnerRemoved */21](user2, /* :: */[
              user1,
              /* [] */0
            ], log);
        Caml_oo_curry.js2(710435299, 2, watcher, Generators.Log[/* lastItem */4](log$1));
        return WatcherHelpers.testWatcherHasEventPending("CustodianAccepted", watcher, Generators.Log[/* systemIssuer */3](log$1), (function (param) {
                      if (param.tag === 16) {
                        return Caml_obj.caml_equal(param[0][/* data */3], proposal[/* data */6]);
                      } else {
                        return false;
                      }
                    }));
      }));

var CustodianApproval = 0;

exports.CustodianApproval = CustodianApproval;
/*  Not a pure module */
