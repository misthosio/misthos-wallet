// Generated by BUCKLESCRIPT VERSION 4.0.5, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Belt_Map = require("bs-platform/lib/js/belt_Map.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Js_option = require("bs-platform/lib/js/js_option.js");
var Js_primitive = require("bs-platform/lib/js/js_primitive.js");
var Belt_MapString = require("bs-platform/lib/js/belt_MapString.js");
var PrimitiveTypes = require("../../application/PrimitiveTypes.bs.js");
var PayoutTransaction = require("../../application/wallet/PayoutTransaction.bs.js");

function make() {
  return /* record */[
          /* ventureId */PrimitiveTypes.VentureId[/* fromString */1](""),
          /* payoutProcesses */PrimitiveTypes.ProcessId[/* makeMap */8](/* () */0),
          /* unconfirmedTxs : [] */0,
          /* confirmedTxs : [] */0,
          /* network : Regtest */0,
          /* txDates */Belt_MapString.empty
        ];
}

function mapConfirmation(param, state) {
  var unconfirmedTxs = state[/* unconfirmedTxs */2];
  var txId = param[/* txId */0];
  var txDate = new Date(param[/* unixTime */2] * 1000);
  var newTxs = Belt_List.keepMap(unconfirmedTxs, (function (data) {
          if (data[/* txId */2] === txId) {
            return /* record */[
                    /* txType */data[/* txType */0],
                    /* status : Confirmed */0,
                    /* txId */data[/* txId */2],
                    /* amount */data[/* amount */3],
                    /* date */Js_primitive.some(txDate),
                    /* detailsLink */data[/* detailsLink */5]
                  ];
          }
          
        }));
  var newUnconf = Belt_List.keep(unconfirmedTxs, (function (param) {
          return param[/* txId */2] !== txId;
        }));
  return /* record */[
          /* ventureId */state[/* ventureId */0],
          /* payoutProcesses */state[/* payoutProcesses */1],
          /* unconfirmedTxs */newUnconf,
          /* confirmedTxs */Belt_List.concat(newTxs, state[/* confirmedTxs */3]),
          /* network */state[/* network */4],
          /* txDates */Belt_MapString.set(state[/* txDates */5], txId, txDate)
        ];
}

function apply($$event, state) {
  switch ($$event.tag | 0) {
    case 0 : 
        var match = $$event[0];
        return /* record */[
                /* ventureId */match[/* ventureId */0],
                /* payoutProcesses */state[/* payoutProcesses */1],
                /* unconfirmedTxs */state[/* unconfirmedTxs */2],
                /* confirmedTxs */state[/* confirmedTxs */3],
                /* network */match[/* network */7],
                /* txDates */state[/* txDates */5]
              ];
    case 26 : 
        var match$1 = $$event[0];
        return /* record */[
                /* ventureId */state[/* ventureId */0],
                /* payoutProcesses */Belt_Map.set(state[/* payoutProcesses */1], match$1[/* processId */0], match$1[/* data */6][/* payoutTx */1]),
                /* unconfirmedTxs */state[/* unconfirmedTxs */2],
                /* confirmedTxs */state[/* confirmedTxs */3],
                /* network */state[/* network */4],
                /* txDates */state[/* txDates */5]
              ];
    case 34 : 
        var match$2 = $$event[0];
        var txId = match$2[/* txId */1];
        var processId = match$2[/* processId */0];
        var payoutTx = Belt_Map.getExn(state[/* payoutProcesses */1], processId);
        var txDate = Belt_MapString.get(state[/* txDates */5], txId);
        var match$3 = Js_option.isSome(txDate);
        var payout_001 = /* status */match$3 ? /* Confirmed */0 : /* Unconfirmed */1;
        var payout_003 = /* amount */PayoutTransaction.summary(state[/* network */4], payoutTx)[/* spentWithFees */2];
        var payout_005 = /* detailsLink : Venture */Block.__(0, [
            state[/* ventureId */0],
            /* Payout */Block.__(1, [processId])
          ]);
        var payout = /* record */[
          /* txType : Payout */1,
          payout_001,
          /* txId */txId,
          payout_003,
          /* date */txDate,
          payout_005
        ];
        var match$4 = payout_001;
        if (match$4) {
          return /* record */[
                  /* ventureId */state[/* ventureId */0],
                  /* payoutProcesses */state[/* payoutProcesses */1],
                  /* unconfirmedTxs : :: */[
                    payout,
                    state[/* unconfirmedTxs */2]
                  ],
                  /* confirmedTxs */state[/* confirmedTxs */3],
                  /* network */state[/* network */4],
                  /* txDates */state[/* txDates */5]
                ];
        } else {
          return /* record */[
                  /* ventureId */state[/* ventureId */0],
                  /* payoutProcesses */state[/* payoutProcesses */1],
                  /* unconfirmedTxs */state[/* unconfirmedTxs */2],
                  /* confirmedTxs : :: */[
                    payout,
                    state[/* confirmedTxs */3]
                  ],
                  /* network */state[/* network */4],
                  /* txDates */state[/* txDates */5]
                ];
        }
    case 41 : 
        var match$5 = $$event[0];
        var txId$1 = match$5[/* txId */2];
        return /* record */[
                /* ventureId */state[/* ventureId */0],
                /* payoutProcesses */state[/* payoutProcesses */1],
                /* unconfirmedTxs : :: */[
                  /* record */[
                    /* txType : Income */0,
                    /* status : Unconfirmed */1,
                    /* txId */txId$1,
                    /* amount */match$5[/* amount */4],
                    /* date */undefined,
                    /* detailsLink : Venture */Block.__(0, [
                        state[/* ventureId */0],
                        /* Income */Block.__(2, [txId$1])
                      ])
                  ],
                  state[/* unconfirmedTxs */2]
                ],
                /* confirmedTxs */state[/* confirmedTxs */3],
                /* network */state[/* network */4],
                /* txDates */state[/* txDates */5]
              ];
    case 43 : 
        return mapConfirmation($$event[0], state);
    default:
      return state;
  }
}

exports.make = make;
exports.mapConfirmation = mapConfirmation;
exports.apply = apply;
/* PrimitiveTypes Not a pure module */
