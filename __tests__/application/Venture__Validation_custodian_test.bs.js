// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var Jest = require("@glennsl/bs-jest/src/jest.js");
var $$Event = require("../../src/application/events/Event.bs.js");
var Generators = require("../helpers/Generators.bs.js");
var WalletTypes = require("../../src/application/wallet/WalletTypes.bs.js");
var PrimitiveTypes = require("../../src/application/PrimitiveTypes.bs.js");
var ValidationHelpers = require("../helpers/ValidationHelpers.bs.js");
var Venture__Validation = require("../../src/application/Venture__Validation.bs.js");

Jest.describe("CustodianProposed", (function (param) {
        Jest.describe("when proposing a custodian", (function (param) {
                var match = Generators.twoUserSessions(/* () */0);
                var user2 = match[1];
                var user1 = match[0];
                var log = Generators.Log[/* withPartner */17](undefined, user2, /* :: */[
                      user1,
                      /* [] */0
                    ], Generators.Log[/* withCustodian */33](user1, /* :: */[
                          user1,
                          /* [] */0
                        ], Generators.Log[/* withAccount */27](user1, Generators.Log[/* withFirstPartner */18](user1)(Generators.Log[/* createVenture */11](user1)))));
                return ValidationHelpers.testValidationResult(undefined, ValidationHelpers.constructState(undefined, log), Generators.Log[/* lastItem */4](Generators.Log[/* withCustodianProposed */28](user1, user2, log)), /* Ok */0);
              }));
        Jest.describe("when proposing a custodian after removal", (function (param) {
                var match = Generators.twoUserSessions(/* () */0);
                var user2 = match[1];
                var user1 = match[0];
                var log = Generators.Log[/* withCustodianRemoved */37](user2, /* :: */[
                      user1,
                      /* :: */[
                        user2,
                        /* [] */0
                      ]
                    ], Generators.Log[/* withCustodian */33](user2, /* :: */[
                          user1,
                          /* :: */[
                            user2,
                            /* [] */0
                          ]
                        ], Generators.Log[/* withPartner */17](undefined, user2, /* :: */[
                              user1,
                              /* [] */0
                            ], Generators.Log[/* withCustodian */33](user1, /* :: */[
                                  user1,
                                  /* [] */0
                                ], Generators.Log[/* withAccount */27](user1, Generators.Log[/* withFirstPartner */18](user1)(Generators.Log[/* createVenture */11](user1)))))));
                return ValidationHelpers.testValidationResult(undefined, ValidationHelpers.constructState(undefined, log), Generators.Log[/* lastItem */4](Generators.Log[/* withCustodianProposed */28](user1, user2, log)), /* Ok */0);
              }));
        return Jest.describe("validateCustodianData", (function (param) {
                      Jest.describe("when the custodian is not a partner", (function (param) {
                              var match = Generators.threeUserSessions(/* () */0);
                              var user1 = match[0];
                              var log = Generators.Log[/* withPartner */17](undefined, match[1], /* :: */[
                                    user1,
                                    /* [] */0
                                  ], Generators.Log[/* withAccount */27](user1, Generators.Log[/* withFirstPartner */18](user1)(Generators.Log[/* createVenture */11](user1))));
                              var partnerApproval = $$Event.getPartnerAcceptedExn(Generators.Log[/* lastEvent */5](log));
                              return ValidationHelpers.testDataValidation(Venture__Validation.validateCustodianData, ValidationHelpers.constructState(undefined, log), /* record */[
                                          /* partnerId */match[2][/* userId */0],
                                          /* partnerApprovalProcess */partnerApproval[/* processId */0],
                                          /* lastCustodianRemovalProcess */undefined,
                                          /* accountIdx */WalletTypes.AccountIndex[/* default */11]
                                        ], /* BadData */["Partner approval process doesn't match user id"]);
                            }));
                      Jest.describe("when the partner approval process reference is wrong", (function (param) {
                              var match = Generators.twoUserSessions(/* () */0);
                              var user2 = match[1];
                              var user1 = match[0];
                              var log = Generators.Log[/* withPartner */17](undefined, user2, /* :: */[
                                    user1,
                                    /* [] */0
                                  ], Generators.Log[/* withAccount */27](user1, Generators.Log[/* withFirstPartner */18](user1)(Generators.Log[/* createVenture */11](user1))));
                              return ValidationHelpers.testDataValidation(Venture__Validation.validateCustodianData, ValidationHelpers.constructState(undefined, log), /* record */[
                                          /* partnerId */user2[/* userId */0],
                                          /* partnerApprovalProcess */PrimitiveTypes.ProcessId[/* make */10](/* () */0),
                                          /* lastCustodianRemovalProcess */undefined,
                                          /* accountIdx */WalletTypes.AccountIndex[/* default */11]
                                        ], /* BadData */["partner approval process doesn't exist"]);
                            }));
                      Jest.describe("when the account doesn't exist", (function (param) {
                              var match = Generators.twoUserSessions(/* () */0);
                              var user1 = match[0];
                              var log = Generators.Log[/* withFirstPartner */18](user1)(Generators.Log[/* createVenture */11](user1));
                              var partnerApproval = $$Event.getPartnerAcceptedExn(Generators.Log[/* lastEvent */5](log));
                              return ValidationHelpers.testDataValidation(Venture__Validation.validateCustodianData, ValidationHelpers.constructState(undefined, log), /* record */[
                                          /* partnerId */user1[/* userId */0],
                                          /* partnerApprovalProcess */partnerApproval[/* processId */0],
                                          /* lastCustodianRemovalProcess */undefined,
                                          /* accountIdx */WalletTypes.AccountIndex[/* default */11]
                                        ], /* BadData */["account doesn't exist"]);
                            }));
                      return Jest.describe("when lastCustodianRemovalProcess doesn't match", (function (param) {
                                    var match = Generators.twoUserSessions(/* () */0);
                                    var user2 = match[1];
                                    var user1 = match[0];
                                    var log = Generators.Log[/* withPartner */17](undefined, user2, /* :: */[
                                          user1,
                                          /* [] */0
                                        ], Generators.Log[/* withCustodian */33](user1, /* :: */[
                                              user1,
                                              /* [] */0
                                            ], Generators.Log[/* withAccount */27](user1, Generators.Log[/* withFirstPartner */18](user1)(Generators.Log[/* createVenture */11](user1)))));
                                    var partnerApproval = $$Event.getPartnerAcceptedExn(Generators.Log[/* lastEvent */5](log));
                                    var log$1 = Generators.Log[/* withCustodianRemoved */37](user2, /* :: */[
                                          user1,
                                          /* [] */0
                                        ], Generators.Log[/* withCustodian */33](user2, /* :: */[
                                              user1,
                                              /* :: */[
                                                user2,
                                                /* [] */0
                                              ]
                                            ], log));
                                    return ValidationHelpers.testDataValidation(Venture__Validation.validateCustodianData, ValidationHelpers.constructState(undefined, log$1), /* record */[
                                                /* partnerId */user2[/* userId */0],
                                                /* partnerApprovalProcess */partnerApproval[/* processId */0],
                                                /* lastCustodianRemovalProcess */undefined,
                                                /* accountIdx */WalletTypes.AccountIndex[/* default */11]
                                              ], /* BadData */["Last removal doesn't match"]);
                                  }));
                    }));
      }));

/*  Not a pure module */
