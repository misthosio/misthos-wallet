// Generated by BUCKLESCRIPT VERSION 3.0.0, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Event = require("./events/Event.bs.js");
var Policy = require("./Policy.bs.js");
var Address = require("./wallet/Address.bs.js");
var Network = require("./wallet/Network.bs.js");
var WalletTypes = require("./wallet/WalletTypes.bs.js");
var PrimitiveTypes = require("./PrimitiveTypes.bs.js");
var AccountKeyChain = require("./wallet/AccountKeyChain.bs.js");
var PayoutTransaction = require("./wallet/PayoutTransaction.bs.js");

function make() {
  return /* record */[
          /* ventureId */PrimitiveTypes.VentureId[/* fromString */1](""),
          /* network : Testnet */1,
          /* payoutPolicy */Policy.unanimous,
          /* accountKeyChains : [] */0,
          /* exposedCoordinates : [] */0,
          /* reservedInputs : [] */0,
          /* payoutProcesses : [] */0
        ];
}

function getExposedAddresses(param) {
  var accountKeyChains = param[/* accountKeyChains */3];
  return List.map((function (a) {
                return a[/* address */5];
              }), List.map((function (coordinates) {
                    return Address.find(coordinates, accountKeyChains);
                  }), param[/* exposedCoordinates */4]));
}

function apply($$event, state) {
  switch ($$event.tag | 0) {
    case 0 : 
        var match = $$event[0];
        return /* record */[
                /* ventureId */match[/* ventureId */0],
                /* network */match[/* network */6],
                /* payoutPolicy */match[/* metaPolicy */4],
                /* accountKeyChains */state[/* accountKeyChains */3],
                /* exposedCoordinates */state[/* exposedCoordinates */4],
                /* reservedInputs */state[/* reservedInputs */5],
                /* payoutProcesses */state[/* payoutProcesses */6]
              ];
    case 16 : 
        var match$1 = $$event[0];
        var data = match$1[/* data */5];
        var match$2 = data[/* changeAddressCoordinates */2];
        return /* record */[
                /* ventureId */state[/* ventureId */0],
                /* network */state[/* network */1],
                /* payoutPolicy */state[/* payoutPolicy */2],
                /* accountKeyChains */state[/* accountKeyChains */3],
                /* exposedCoordinates */match$2 ? /* :: */[
                    match$2[0],
                    state[/* exposedCoordinates */4]
                  ] : state[/* exposedCoordinates */4],
                /* reservedInputs */List.rev_append(List.map((function (prim) {
                            return prim[1];
                          }), data[/* payoutTx */1][/* usedInputs */1]), state[/* reservedInputs */5]),
                /* payoutProcesses : :: */[
                  /* tuple */[
                    match$1[/* processId */0],
                    data[/* payoutTx */1]
                  ],
                  state[/* payoutProcesses */6]
                ]
              ];
    case 20 : 
        var payoutTx = List.assoc($$event[0][/* processId */0], state[/* payoutProcesses */6]);
        return /* record */[
                /* ventureId */state[/* ventureId */0],
                /* network */state[/* network */1],
                /* payoutPolicy */state[/* payoutPolicy */2],
                /* accountKeyChains */state[/* accountKeyChains */3],
                /* exposedCoordinates */state[/* exposedCoordinates */4],
                /* reservedInputs */List.filter((function (input) {
                          return List.exists((function (i) {
                                        if (input[/* txId */0] === i[/* txId */0]) {
                                          return input[/* txOutputN */1] === i[/* txOutputN */1];
                                        } else {
                                          return false;
                                        }
                                      }), List.map((function (prim) {
                                            return prim[1];
                                          }), payoutTx[/* usedInputs */1])) === false;
                        }))(state[/* reservedInputs */5]),
                /* payoutProcesses */state[/* payoutProcesses */6]
              ];
    case 22 : 
        var payoutTx$1 = List.assoc($$event[0][/* processId */0], state[/* payoutProcesses */6]);
        return /* record */[
                /* ventureId */state[/* ventureId */0],
                /* network */state[/* network */1],
                /* payoutPolicy */state[/* payoutPolicy */2],
                /* accountKeyChains */state[/* accountKeyChains */3],
                /* exposedCoordinates */state[/* exposedCoordinates */4],
                /* reservedInputs */List.filter((function (input) {
                          return List.exists((function (i) {
                                        if (input[/* txId */0] === i[/* txId */0]) {
                                          return input[/* txOutputN */1] === i[/* txOutputN */1];
                                        } else {
                                          return false;
                                        }
                                      }), List.map((function (prim) {
                                            return prim[1];
                                          }), payoutTx$1[/* usedInputs */1])) === false;
                        }))(state[/* reservedInputs */5]),
                /* payoutProcesses */state[/* payoutProcesses */6]
              ];
    case 24 : 
        return /* record */[
                /* ventureId */state[/* ventureId */0],
                /* network */state[/* network */1],
                /* payoutPolicy */state[/* payoutPolicy */2],
                /* accountKeyChains */AccountKeyChain.Collection[/* add */1]($$event[0][/* keyChain */0], state[/* accountKeyChains */3]),
                /* exposedCoordinates */state[/* exposedCoordinates */4],
                /* reservedInputs */state[/* reservedInputs */5],
                /* payoutProcesses */state[/* payoutProcesses */6]
              ];
    case 25 : 
        return /* record */[
                /* ventureId */state[/* ventureId */0],
                /* network */state[/* network */1],
                /* payoutPolicy */state[/* payoutPolicy */2],
                /* accountKeyChains */state[/* accountKeyChains */3],
                /* exposedCoordinates : :: */[
                  $$event[0][/* coordinates */0],
                  state[/* exposedCoordinates */4]
                ],
                /* reservedInputs */state[/* reservedInputs */5],
                /* payoutProcesses */state[/* payoutProcesses */6]
              ];
    default:
      return state;
  }
}

function exposeNextIncomeAddress(userId, accountIdx, param) {
  var accountKeyChain = AccountKeyChain.Collection[/* latest */3](accountIdx, param[/* accountKeyChains */3]);
  var coordinates = Address.Coordinates[/* nextExternal */2](userId, param[/* exposedCoordinates */4], accountKeyChain);
  return Event.IncomeAddressExposed[/* make */0](coordinates, Address.make(coordinates, accountKeyChain)[/* address */5]);
}

function preparePayoutTx(param, accountIdx, destinations, satsPerByte, param$1) {
  var reservedInputs = param$1[/* reservedInputs */5];
  var accountKeyChains = param$1[/* accountKeyChains */3];
  var payoutPolicy = param$1[/* payoutPolicy */2];
  var ventureId = param$1[/* ventureId */0];
  var network = param[/* network */5];
  var masterKeyChain = param[/* masterKeyChain */4];
  var userId = param[/* userId */0];
  var accountKeyChain = AccountKeyChain.Collection[/* latest */3](accountIdx, accountKeyChains);
  var currentKeyChainIdx = accountKeyChain[/* keyChainIdx */1];
  var coordinates = Address.Coordinates[/* allForAccount */8](accountIdx)(param$1[/* exposedCoordinates */4]);
  var nextChangeCoordinates = Address.Coordinates[/* nextInternal */1](userId, coordinates, accountKeyChain);
  return Network.transactionInputs(network)(coordinates, accountKeyChains).then((function (inputs) {
                var inputs$1 = List.filter((function (input) {
                          return List.exists((function (reservedIn) {
                                        if (reservedIn[/* txId */0] === input[/* txId */0]) {
                                          return reservedIn[/* txOutputN */1] === input[/* txOutputN */1];
                                        } else {
                                          return false;
                                        }
                                      }), reservedInputs) === false;
                        }))(inputs);
                var oldInputs = List.find_all((function (i) {
                          return WalletTypes.AccountKeyChainIndex[/* neq */7](currentKeyChainIdx, Address.Coordinates[/* keyChainIdx */4](i[/* coordinates */6]));
                        }))(inputs$1);
                var changeAddress = Address.find(nextChangeCoordinates, accountKeyChains);
                var match = PayoutTransaction.build(oldInputs, inputs$1, destinations, satsPerByte, changeAddress, network);
                var match$1;
                match$1 = match.tag ? /* tuple */[
                    match[0],
                    /* None */0
                  ] : /* tuple */[
                    match[0],
                    /* Some */[nextChangeCoordinates]
                  ];
                var payoutTx = match$1[0];
                var match$2 = PayoutTransaction.signPayout(ventureId, userId, masterKeyChain, accountKeyChains, payoutTx, network);
                var payoutTx$1 = match$2 ? match$2[0] : payoutTx;
                return Promise.resolve(Curry._5(Event.Payout[/* Proposed */2][/* make */0], /* None */0, /* None */0, userId, payoutPolicy, /* record */[
                                /* accountIdx */accountIdx,
                                /* payoutTx */payoutTx$1,
                                /* changeAddressCoordinates */match$1[1]
                              ]));
              }));
}

exports.make = make;
exports.getExposedAddresses = getExposedAddresses;
exports.apply = apply;
exports.exposeNextIncomeAddress = exposeNextIncomeAddress;
exports.preparePayoutTx = preparePayoutTx;
/* Event Not a pure module */
