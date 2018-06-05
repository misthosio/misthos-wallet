// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE
'use strict';

var Event = require("../../src/application/events/Event.bs.js");
var Fixtures = require("../helpers/Fixtures.bs.js");
var Generators = require("../helpers/Generators.bs.js");
var WalletTypes = require("../../src/application/wallet/WalletTypes.bs.js");
var PrimitiveTypes = require("../../src/application/PrimitiveTypes.bs.js");
var ValidationHelpers = require("../helpers/ValidationHelpers.bs.js");
var Venture__Validation = require("../../src/application/Venture__Validation.bs.js");

describe("CustodianKeyChainUpdated", (function () {
        Fixtures.withCached(/* None */0, "CustodianKeyChainUpdated", "when everything is fine", (function () {
                return Generators.withUserSessions(2);
              }), (function (sessions) {
                var match = Generators.twoUserSessionsFromArray(sessions);
                var user1 = match[0];
                return Generators.Log[/* withCustodian */33](user1, /* :: */[
                            user1,
                            /* [] */0
                          ], Generators.Log[/* withAccount */27](user1, Generators.Log[/* withFirstPartner */18](user1)(Generators.Log[/* createVenture */11](user1))));
              }), (function (sessions, log) {
                var match = Generators.twoUserSessionsFromArray(sessions);
                return ValidationHelpers.testValidationResult(/* None */0, ValidationHelpers.constructState(/* None */0, log), Generators.Log[/* lastItem */4](Generators.Log[/* withCustodianKeyChain */38](/* None */0, /* None */0, match[0], log)), /* Ok */0);
              }));
        Fixtures.withCached(/* None */0, "CustodianKeyChainUpdated", "when the signer doesn't match the custodianId", (function () {
                return Generators.withUserSessions(2);
              }), (function (sessions) {
                var match = Generators.twoUserSessionsFromArray(sessions);
                var user1 = match[0];
                return Generators.Log[/* withPartner */17](/* None */0, match[1], /* :: */[
                            user1,
                            /* [] */0
                          ], Generators.Log[/* withCustodian */33](user1, /* :: */[
                                user1,
                                /* [] */0
                              ], Generators.Log[/* withAccount */27](user1, Generators.Log[/* withFirstPartner */18](user1)(Generators.Log[/* createVenture */11](user1)))));
              }), (function (sessions, log) {
                var match = Generators.twoUserSessionsFromArray(sessions);
                return ValidationHelpers.testValidationResult(/* None */0, ValidationHelpers.constructState(/* None */0, log), Generators.Log[/* lastItem */4](Generators.Log[/* withCustodianKeyChain */38](/* None */0, /* Some */[match[1]], match[0], log)), /* InvalidIssuer */2);
              }));
        Fixtures.withCached(/* None */0, "CustodianKeyChainUpdated", "when the custodianApprovalProcess doesn't exist", (function () {
                return Generators.withUserSessions(2);
              }), (function (sessions) {
                var match = Generators.twoUserSessionsFromArray(sessions);
                var user1 = match[0];
                return Generators.Log[/* withCustodian */33](user1, /* :: */[
                            user1,
                            /* [] */0
                          ], Generators.Log[/* withAccount */27](user1, Generators.Log[/* withFirstPartner */18](user1)(Generators.Log[/* createVenture */11](user1))));
              }), (function (sessions, log) {
                var match = Generators.twoUserSessionsFromArray(sessions);
                var user1 = match[0];
                return ValidationHelpers.testDataValidation((function (param, param$1) {
                              return ValidationHelpers.withIssuer(user1, Venture__Validation.validateCustodianKeyChainUpdated, param, param$1);
                            }), ValidationHelpers.constructState(/* None */0, log), /* record */[
                            /* custodianApprovalProcess */PrimitiveTypes.ProcessId[/* make */10](/* () */0),
                            /* custodianId */user1[/* userId */0],
                            /* keyChain */Generators.custodianKeyChain(/* None */0, Generators.Log[/* ventureId */2](log), 0, user1)
                          ], /* BadData */["Bad custodianApprovalProcess"]);
              }));
        Fixtures.withCached(/* None */0, "CustodianKeyChainUpdated", "when the custodianApprovalProcess isn't completed", (function () {
                return Generators.withUserSessions(2);
              }), (function (sessions) {
                var match = Generators.twoUserSessionsFromArray(sessions);
                var user1 = match[0];
                return Generators.Log[/* withCustodianProposed */28](user1, user1, Generators.Log[/* withAccount */27](user1, Generators.Log[/* withFirstPartner */18](user1)(Generators.Log[/* createVenture */11](user1))));
              }), (function (sessions, log) {
                var match = Generators.twoUserSessionsFromArray(sessions);
                var user1 = match[0];
                var proposal = Event.getCustodianProposedExn(Generators.Log[/* lastEvent */5](log));
                return ValidationHelpers.testDataValidation((function (param, param$1) {
                              return ValidationHelpers.withIssuer(user1, Venture__Validation.validateCustodianKeyChainUpdated, param, param$1);
                            }), ValidationHelpers.constructState(/* None */0, log), /* record */[
                            /* custodianApprovalProcess */proposal[/* processId */0],
                            /* custodianId */user1[/* userId */0],
                            /* keyChain */Generators.custodianKeyChain(/* None */0, Generators.Log[/* ventureId */2](log), 0, user1)
                          ], /* BadData */["Bad custodianApprovalProcess"]);
              }));
        Fixtures.withCached(/* None */0, "CustodianKeyChainUpdated", "when the custodian approval process is for another user", (function () {
                return Generators.withUserSessions(2);
              }), (function (sessions) {
                var match = Generators.twoUserSessionsFromArray(sessions);
                var user1 = match[0];
                return Generators.Log[/* withCustodian */33](user1, /* :: */[
                            user1,
                            /* [] */0
                          ], Generators.Log[/* withAccount */27](user1, Generators.Log[/* withFirstPartner */18](user1)(Generators.Log[/* createVenture */11](user1))));
              }), (function (sessions, log) {
                var match = Generators.twoUserSessionsFromArray(sessions);
                var user2 = match[1];
                var user1 = match[0];
                var accepted = Event.getCustodianAcceptedExn(Generators.Log[/* lastEvent */5](log));
                var log$1 = Generators.Log[/* withPartner */17](/* None */0, user2, /* :: */[
                      user1,
                      /* [] */0
                    ], log);
                return ValidationHelpers.testDataValidation((function (param, param$1) {
                              return ValidationHelpers.withIssuer(user2, Venture__Validation.validateCustodianKeyChainUpdated, param, param$1);
                            }), ValidationHelpers.constructState(/* None */0, log$1), /* record */[
                            /* custodianApprovalProcess */accepted[/* processId */0],
                            /* custodianId */user2[/* userId */0],
                            /* keyChain */Generators.custodianKeyChain(/* None */0, Generators.Log[/* ventureId */2](log$1), 0, user1)
                          ], /* BadData */["CustodianApprovalProcess is for another partner"]);
              }));
        Fixtures.withCached(/* None */0, "CustodianKeyChainUpdated", "when the account doesn't exist", (function () {
                return Generators.withUserSessions(2);
              }), (function (sessions) {
                var match = Generators.twoUserSessionsFromArray(sessions);
                var user1 = match[0];
                return Generators.Log[/* withCustodian */33](user1, /* :: */[
                            user1,
                            /* [] */0
                          ], Generators.Log[/* withAccount */27](user1, Generators.Log[/* withFirstPartner */18](user1)(Generators.Log[/* createVenture */11](user1))));
              }), (function (sessions, log) {
                var match = Generators.twoUserSessionsFromArray(sessions);
                var user1 = match[0];
                var accepted = Event.getCustodianAcceptedExn(Generators.Log[/* lastEvent */5](log));
                return ValidationHelpers.testDataValidation((function (param, param$1) {
                              return ValidationHelpers.withIssuer(user1, Venture__Validation.validateCustodianKeyChainUpdated, param, param$1);
                            }), ValidationHelpers.constructState(/* None */0, log), /* record */[
                            /* custodianApprovalProcess */accepted[/* processId */0],
                            /* custodianId */user1[/* userId */0],
                            /* keyChain */Generators.custodianKeyChain(/* Some */[WalletTypes.AccountIndex[/* fromInt */1](1)], Generators.Log[/* ventureId */2](log), 0, user1)
                          ], /* BadData */["Account doesn't exist"]);
              }));
        return Fixtures.withCached(/* None */0, "CustodianKeyChainUpdated", "when the key chain index isn't in order", (function () {
                      return Generators.withUserSessions(2);
                    }), (function (sessions) {
                      var match = Generators.twoUserSessionsFromArray(sessions);
                      var user1 = match[0];
                      return Generators.Log[/* withCustodian */33](user1, /* :: */[
                                  user1,
                                  /* [] */0
                                ], Generators.Log[/* withAccount */27](user1, Generators.Log[/* withFirstPartner */18](user1)(Generators.Log[/* createVenture */11](user1))));
                    }), (function (sessions, log) {
                      var match = Generators.twoUserSessionsFromArray(sessions);
                      var user1 = match[0];
                      var accepted = Event.getCustodianAcceptedExn(Generators.Log[/* lastEvent */5](log));
                      return ValidationHelpers.testDataValidation((function (param, param$1) {
                                    return ValidationHelpers.withIssuer(user1, Venture__Validation.validateCustodianKeyChainUpdated, param, param$1);
                                  }), ValidationHelpers.constructState(/* None */0, log), /* record */[
                                  /* custodianApprovalProcess */accepted[/* processId */0],
                                  /* custodianId */user1[/* userId */0],
                                  /* keyChain */Generators.custodianKeyChain(/* None */0, Generators.Log[/* ventureId */2](log), 1, user1)
                                ], /* BadData */["CustodianKeyChainIndex isn't in order"]);
                    }));
      }));

/*  Not a pure module */
