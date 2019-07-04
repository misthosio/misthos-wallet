// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var $$Event = require("../../../src/application/events/Event.bs.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Fixtures = require("../../helpers/Fixtures.bs.js");
var Generators = require("../../helpers/Generators.bs.js");
var WalletTypes = require("../../../src/application/wallet/WalletTypes.bs.js");
var WatcherHelpers = require("../../helpers/WatcherHelpers.bs.js");
var CustodianKeyChain = require("../../../src/application/wallet/CustodianKeyChain.bs.js");
var Watcher__AccountKeyChain = require("../../../src/application/watcher/Watcher__AccountKeyChain.bs.js");

function keyChainEq(keyChainA, keyChainB) {
  return Caml_obj.caml_equal(CustodianKeyChain.encode(keyChainA), CustodianKeyChain.encode(keyChainB));
}

describe("Watcher__AccountKeyChain", (function () {
        describe("Identifies a key chain when a custodian key chain changes", (function () {
                var user1 = Fixtures.threeUserSessions[0];
                var log = Generators.Log[/* withAccount */27](user1, Generators.Log[/* withFirstPartner */18](user1)(Fixtures.createVenture(user1)));
                var acceptance = $$Event.getAccountCreationAcceptedExn(Generators.Log[/* lastEvent */5](log));
                var log$1 = Generators.Log[/* withCustodianKeyChain */38](undefined, undefined, undefined, user1, Generators.Log[/* withCustodian */33](user1, /* :: */[
                          user1,
                          /* [] */0
                        ], log));
                var watcher = Watcher__AccountKeyChain.make(user1, acceptance, Generators.Log[/* eventLog */6](log$1));
                return WatcherHelpers.testWatcherHasEventPending("AccountKeyChainIdentified", watcher, Generators.Log[/* systemIssuer */3](log$1), (function (param) {
                              if (param.tag === 38) {
                                var match = param[0][/* keyChain */0];
                                if (WalletTypes.AccountIndex[/* eq */7](match[/* accountIdx */0], WalletTypes.AccountIndex[/* default */11])) {
                                  return match[/* identifier */1] === "8974ad69910afdca42d4c7c08c094c8d2a9d454d0f02b5b101eb7abd30a06d30";
                                } else {
                                  return false;
                                }
                              } else {
                                return false;
                              }
                            }));
              }));
        describe("Identifies a key chain when a partner is removed", (function () {
                var user2 = Fixtures.threeUserSessions[1];
                var user1 = Fixtures.threeUserSessions[0];
                var log = Generators.Log[/* withAccount */27](user1, Generators.Log[/* withFirstPartner */18](user1)(Fixtures.createVenture(user1)));
                var acceptance = $$Event.getAccountCreationAcceptedExn(Generators.Log[/* lastEvent */5](log));
                var eta = Generators.Log[/* withCustodianKeyChain */38](undefined, undefined, undefined, user1, Generators.Log[/* withCustodian */33](user1, /* :: */[
                          user1,
                          /* [] */0
                        ], log));
                var eta$1 = Generators.Log[/* withCustodianKeyChain */38](undefined, undefined, undefined, user2, Generators.Log[/* withCustodian */33](user2, /* :: */[
                          user1,
                          /* :: */[
                            user2,
                            /* [] */0
                          ]
                        ], Generators.Log[/* withPartner */17](undefined, user2, /* :: */[
                              user1,
                              /* [] */0
                            ], Generators.Log[/* withAccountKeyChainActivated */40](undefined, user1, Generators.Log[/* withAccountKeyChainIdentified */39](undefined, eta)))));
                var log$1 = Generators.Log[/* withCustodianKeyChain */38](1, undefined, undefined, user1, Generators.Log[/* withPartnerRemoved */23](user2, /* :: */[
                          user1,
                          /* [] */0
                        ], Generators.Log[/* withCustodianRemoved */37](user2, /* :: */[
                              user1,
                              /* [] */0
                            ], Generators.Log[/* withAccountKeyChainActivated */40](undefined, user1, Generators.Log[/* withAccountKeyChainIdentified */39](undefined, eta$1)))));
                var watcher = Watcher__AccountKeyChain.make(user1, acceptance, Generators.Log[/* eventLog */6](log$1));
                return WatcherHelpers.testWatcherHasEventPending("AccountKeyChainIdentified", watcher, Generators.Log[/* systemIssuer */3](log$1), (function (param) {
                              if (param.tag === 38) {
                                var match = param[0][/* keyChain */0];
                                if (WalletTypes.AccountIndex[/* eq */7](match[/* accountIdx */0], WalletTypes.AccountIndex[/* default */11]) && match[/* identifier */1] === "84ba3d5f75b50fc70cdebf3b637c31c81d820519a68fd997025296ba765f2dc5") {
                                  return List.length(match[/* custodianKeyChains */4]) === 1;
                                } else {
                                  return false;
                                }
                              } else {
                                return false;
                              }
                            }));
              }));
        describe("Activates a key chain when a custodian key chain changes", (function () {
                var user1 = Fixtures.threeUserSessions[0];
                var log = Generators.Log[/* withAccount */27](user1, Generators.Log[/* withFirstPartner */18](user1)(Fixtures.createVenture(user1)));
                var acceptance = $$Event.getAccountCreationAcceptedExn(Generators.Log[/* lastEvent */5](log));
                var eta = Generators.Log[/* withCustodianKeyChain */38](undefined, undefined, undefined, user1, Generators.Log[/* withCustodian */33](user1, /* :: */[
                          user1,
                          /* [] */0
                        ], log));
                var log$1 = Generators.Log[/* withAccountKeyChainIdentified */39](undefined, eta);
                var watcher = Watcher__AccountKeyChain.make(user1, acceptance, Generators.Log[/* eventLog */6](log$1));
                return WatcherHelpers.testWatcherHasEventPending("AccountKeyChainActivated", watcher, user1[/* issuerKeyPair */2], (function (param) {
                              if (param.tag === 39) {
                                var match = param[0];
                                if (WalletTypes.AccountIndex[/* eq */7](match[/* accountIdx */0], WalletTypes.AccountIndex[/* default */11]) && match[/* identifier */2] === "8974ad69910afdca42d4c7c08c094c8d2a9d454d0f02b5b101eb7abd30a06d30") {
                                  return match[/* sequence */3] === 0;
                                } else {
                                  return false;
                                }
                              } else {
                                return false;
                              }
                            }));
              }));
        describe("Is idle when the key chain has been activated", (function () {
                var match = Generators.twoUserSessions(/* () */0);
                var user1 = match[0];
                var log = Generators.Log[/* withAccount */27](user1, Generators.Log[/* withFirstPartner */18](user1)(Fixtures.createVenture(user1)));
                var acceptance = $$Event.getAccountCreationAcceptedExn(Generators.Log[/* lastEvent */5](log));
                var eta = Generators.Log[/* withCustodianKeyChain */38](undefined, undefined, undefined, user1, Generators.Log[/* withCustodian */33](user1, /* :: */[
                          user1,
                          /* [] */0
                        ], log));
                var log$1 = Generators.Log[/* withAccountKeyChainActivated */40](undefined, user1, Generators.Log[/* withAccountKeyChainIdentified */39](undefined, eta));
                return WatcherHelpers.testWatcherHasNoEventPending(Watcher__AccountKeyChain.make(user1, acceptance, Generators.Log[/* eventLog */6](log$1)));
              }));
        describe("Activates a key chain when a custodian is removed", (function () {
                var user2 = Fixtures.threeUserSessions[1];
                var user1 = Fixtures.threeUserSessions[0];
                var log = Generators.Log[/* withAccount */27](user1, Generators.Log[/* withFirstPartner */18](user1)(Fixtures.createVenture(user1)));
                var acceptance = $$Event.getAccountCreationAcceptedExn(Generators.Log[/* lastEvent */5](log));
                var eta = Generators.Log[/* withCustodianKeyChain */38](undefined, undefined, undefined, user1, Generators.Log[/* withCustodian */33](user1, /* :: */[
                          user1,
                          /* [] */0
                        ], log));
                var eta$1 = Generators.Log[/* withCustodianKeyChain */38](undefined, undefined, undefined, user2, Generators.Log[/* withCustodian */33](user2, /* :: */[
                          user1,
                          /* :: */[
                            user2,
                            /* [] */0
                          ]
                        ], Generators.Log[/* withPartner */17](undefined, user2, /* :: */[
                              user1,
                              /* [] */0
                            ], Generators.Log[/* withAccountKeyChainActivated */40](undefined, user1, Generators.Log[/* withAccountKeyChainIdentified */39](undefined, eta)))));
                var log$1 = Generators.Log[/* withCustodianRemoved */37](user2, /* :: */[
                      user1,
                      /* [] */0
                    ], Generators.Log[/* withAccountKeyChainActivated */40](undefined, user1, Generators.Log[/* withAccountKeyChainIdentified */39](undefined, eta$1)));
                var watcher = Watcher__AccountKeyChain.make(user1, acceptance, Generators.Log[/* eventLog */6](log$1));
                return WatcherHelpers.testWatcherHasEventPending("AccountKeyChainActivated", watcher, user1[/* issuerKeyPair */2], (function (param) {
                              if (param.tag === 39) {
                                var match = param[0];
                                if (WalletTypes.AccountIndex[/* eq */7](match[/* accountIdx */0], WalletTypes.AccountIndex[/* default */11]) && match[/* identifier */2] === "8974ad69910afdca42d4c7c08c094c8d2a9d454d0f02b5b101eb7abd30a06d30") {
                                  return match[/* sequence */3] === 1;
                                } else {
                                  return false;
                                }
                              } else {
                                return false;
                              }
                            }));
              }));
        describe("Is idle when the partner is removed", (function () {
                var match = Generators.twoUserSessions(/* () */0);
                var user2 = match[1];
                var user1 = match[0];
                var log = Generators.Log[/* withAccount */27](user1, Generators.Log[/* withFirstPartner */18](user1)(Fixtures.createVenture(user1)));
                var acceptance = $$Event.getAccountCreationAcceptedExn(Generators.Log[/* lastEvent */5](log));
                var eta = Generators.Log[/* withCustodianKeyChain */38](undefined, undefined, undefined, user1, Generators.Log[/* withCustodian */33](user1, /* :: */[
                          user1,
                          /* [] */0
                        ], log));
                var log$1 = Generators.Log[/* withCustodianKeyChain */38](undefined, undefined, undefined, user2, Generators.Log[/* withPartnerRemoved */23](user1, /* :: */[
                          user2,
                          /* [] */0
                        ], Generators.Log[/* withCustodian */33](user2, /* :: */[
                              user1,
                              /* :: */[
                                user2,
                                /* [] */0
                              ]
                            ], Generators.Log[/* withPartner */17](undefined, user2, /* :: */[
                                  user1,
                                  /* [] */0
                                ], Generators.Log[/* withAccountKeyChainActivated */40](undefined, user1, Generators.Log[/* withAccountKeyChainIdentified */39](undefined, eta))))));
                return WatcherHelpers.testWatcherHasNoEventPending(Watcher__AccountKeyChain.make(user1, acceptance, Generators.Log[/* eventLog */6](log$1)));
              }));
        describe("Is idle when the custodian is removed", (function () {
                var match = Generators.twoUserSessions(/* () */0);
                var user2 = match[1];
                var user1 = match[0];
                var log = Generators.Log[/* withAccount */27](user1, Generators.Log[/* withFirstPartner */18](user1)(Fixtures.createVenture(user1)));
                var acceptance = $$Event.getAccountCreationAcceptedExn(Generators.Log[/* lastEvent */5](log));
                var eta = Generators.Log[/* withCustodianKeyChain */38](undefined, undefined, undefined, user1, Generators.Log[/* withCustodian */33](user1, /* :: */[
                          user1,
                          /* [] */0
                        ], log));
                var log$1 = Generators.Log[/* withCustodianKeyChain */38](undefined, undefined, undefined, user2, Generators.Log[/* withCustodianRemoved */37](user1, /* :: */[
                          user2,
                          /* [] */0
                        ], Generators.Log[/* withCustodian */33](user2, /* :: */[
                              user1,
                              /* :: */[
                                user2,
                                /* [] */0
                              ]
                            ], Generators.Log[/* withPartner */17](undefined, user2, /* :: */[
                                  user1,
                                  /* [] */0
                                ], Generators.Log[/* withAccountKeyChainActivated */40](undefined, user1, Generators.Log[/* withAccountKeyChainIdentified */39](undefined, eta))))));
                return WatcherHelpers.testWatcherHasNoEventPending(Watcher__AccountKeyChain.make(user1, acceptance, Generators.Log[/* eventLog */6](log$1)));
              }));
        return /* () */0;
      }));

var KeyChain = 0;

var AccountKeyChain = 0;

exports.KeyChain = KeyChain;
exports.AccountKeyChain = AccountKeyChain;
exports.keyChainEq = keyChainEq;
/*  Not a pure module */
