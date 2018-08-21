// Generated by BUCKLESCRIPT VERSION 4.0.5, PLEASE EDIT WITH CARE
'use strict';

var Utils = require("../../utils/Utils.bs.js");
var Belt_Map = require("bs-platform/lib/js/belt_Map.js");
var Belt_Set = require("bs-platform/lib/js/belt_Set.js");
var Js_option = require("bs-platform/lib/js/js_option.js");
var WalletTypes = require("../../application/wallet/WalletTypes.bs.js");
var Js_primitive = require("bs-platform/lib/js/js_primitive.js");
var PrimitiveTypes = require("../../application/PrimitiveTypes.bs.js");
var CustodianKeyChain = require("../../application/wallet/CustodianKeyChain.bs.js");

function make(localUser) {
  return /* record */[
          /* localUser */localUser,
          /* ledgerIds */WalletTypes.AccountIndex[/* makeMap */10](/* () */0),
          /* ledgerUpToDate */WalletTypes.AccountIndex[/* makeMap */10](/* () */0),
          /* nextKeyChainIdx */WalletTypes.AccountIndex[/* makeMap */10](/* () */0),
          /* ledgerConnected */WalletTypes.AccountIndex[/* makeMap */10](/* () */0)
        ];
}

function ledgerId(accountIdx, param) {
  return Belt_Map.get(param[/* ledgerIds */1], accountIdx);
}

function ledgerUpToDate(accountIdx, param) {
  return Belt_Map.getWithDefault(param[/* ledgerUpToDate */2], accountIdx, false);
}

function nextKeyChainIdx(accountIdx, param) {
  return Belt_Map.getWithDefault(param[/* nextKeyChainIdx */3], accountIdx, WalletTypes.CustodianKeyChainIndex[/* first */10]);
}

function ledgerConnected(accountIdx, param) {
  return Belt_Map.getWithDefault(param[/* ledgerConnected */4], accountIdx, PrimitiveTypes.UserId[/* emptySet */9]);
}

function apply($$event, state) {
  switch ($$event.tag | 0) {
    case 10 : 
        if (PrimitiveTypes.UserId[/* neq */6]($$event[0][/* data */2][/* id */0], state[/* localUser */0])) {
          return /* record */[
                  /* localUser */state[/* localUser */0],
                  /* ledgerIds */state[/* ledgerIds */1],
                  /* ledgerUpToDate */WalletTypes.AccountIndex[/* makeMap */10](/* () */0),
                  /* nextKeyChainIdx */state[/* nextKeyChainIdx */3],
                  /* ledgerConnected */state[/* ledgerConnected */4]
                ];
        } else {
          return state;
        }
    case 37 : 
        var match = $$event[0];
        var keyChain = match[/* keyChain */2];
        var custodianId = match[/* custodianId */1];
        if (PrimitiveTypes.UserId[/* eq */5](custodianId, state[/* localUser */0])) {
          var accountIdx = CustodianKeyChain.accountIdx(keyChain);
          var match$1 = CustodianKeyChain.hardwareId(keyChain);
          var match$2 = CustodianKeyChain.hardwareId(keyChain);
          return /* record */[
                  /* localUser */state[/* localUser */0],
                  /* ledgerIds */match$1 !== undefined ? Belt_Map.set(state[/* ledgerIds */1], accountIdx, match$1) : state[/* ledgerIds */1],
                  /* ledgerUpToDate */Belt_Map.set(state[/* ledgerUpToDate */2], accountIdx, true),
                  /* nextKeyChainIdx */Belt_Map.set(state[/* nextKeyChainIdx */3], accountIdx, WalletTypes.CustodianKeyChainIndex[/* next */2](CustodianKeyChain.keyChainIdx(keyChain))),
                  /* ledgerConnected */match$2 !== undefined ? Belt_Map.updateU(state[/* ledgerConnected */4], accountIdx, (function (users) {
                            return Js_primitive.some(Js_option.getWithDefault(Belt_Set.add(PrimitiveTypes.UserId[/* emptySet */9], custodianId), Utils.mapOption((function (users) {
                                                  return Belt_Set.add(users, custodianId);
                                                }), users)));
                          })) : state[/* ledgerConnected */4]
                ];
        } else {
          return state;
        }
    default:
      return state;
  }
}

exports.make = make;
exports.ledgerId = ledgerId;
exports.ledgerUpToDate = ledgerUpToDate;
exports.nextKeyChainIdx = nextKeyChainIdx;
exports.ledgerConnected = ledgerConnected;
exports.apply = apply;
/* Utils Not a pure module */
