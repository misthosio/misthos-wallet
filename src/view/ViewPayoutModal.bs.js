// Generated by BUCKLESCRIPT VERSION 3.1.4, PLEASE EDIT WITH CARE
'use strict';

var BTC = require("../application/wallet/BTC.bs.js");
var Css = require("bs-css/src/Css.js");
var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Body2 = require("./components/Body2.bs.js");
var Curry = require("bs-platform/lib/js/curry.js");
var React = require("react");
var Voters = require("./components/Voters.bs.js");
var Belt_Set = require("bs-platform/lib/js/belt_Set.js");
var StatusChip = require("./components/StatusChip.bs.js");
var ViewCommon = require("./ViewCommon.bs.js");
var MTypography = require("./components/MTypography.bs.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");
var PrimitiveTypes = require("../application/PrimitiveTypes.bs.js");
var MaterialUi_Table = require("@jsiebern/bs-material-ui/src/MaterialUi_Table.bs.js");
var MaterialUi_TableRow = require("@jsiebern/bs-material-ui/src/MaterialUi_TableRow.bs.js");
var MaterialUi_TableBody = require("@jsiebern/bs-material-ui/src/MaterialUi_TableBody.bs.js");
var MaterialUi_TableCell = require("@jsiebern/bs-material-ui/src/MaterialUi_TableCell.bs.js");
var MaterialUi_Typography = require("@jsiebern/bs-material-ui/src/MaterialUi_Typography.bs.js");
var ProcessApprovalButtons = require("./components/ProcessApprovalButtons.bs.js");
var MaterialUi_SnackbarContent = require("@jsiebern/bs-material-ui/src/MaterialUi_SnackbarContent.bs.js");

var component = ReasonReact.statelessComponent("ViewPayoutModal");

var total = Css.style(/* :: */[
      Css.display(/* flex */-1010954439),
      /* :: */[
        Css.justifyContent(Css.spaceBetween),
        /* :: */[
          Css.alignItems(/* baseline */287825029),
          /* [] */0
        ]
      ]
    ]);

var noBorder = Css.style(/* :: */[
      Css.borderColor(/* transparent */582626130),
      /* [] */0
    ]);

var Styles = /* module */[
  /* total */total,
  /* noBorder */noBorder
];

function make(viewData, commands, cmdStatus, _) {
  return /* record */[
          /* debugName */component[/* debugName */0],
          /* reactClassInternal */component[/* reactClassInternal */1],
          /* handedOffState */component[/* handedOffState */2],
          /* willReceiveProps */component[/* willReceiveProps */3],
          /* didMount */component[/* didMount */4],
          /* didUpdate */component[/* didUpdate */5],
          /* willUnmount */component[/* willUnmount */6],
          /* willUpdate */component[/* willUpdate */7],
          /* shouldUpdate */component[/* shouldUpdate */8],
          /* render */(function () {
              var match = viewData[/* payout */0];
              var match$1 = match[/* data */5];
              var date = match$1[/* date */3];
              var txId = match$1[/* txId */2];
              var summary = match$1[/* summary */1];
              var status = match$1[/* payoutStatus */0];
              var processId = match[/* processId */0];
              var destinationList = $$Array.of_list(List.mapi((function (idx, param) {
                          return ReasonReact.element(/* Some */[String(idx)], /* None */0, MaterialUi_TableRow.make(/* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[
                                          ReasonReact.element(/* None */0, /* None */0, MaterialUi_TableCell.make(/* Some */[noBorder], /* None */0, /* None */0, /* Some */[/* None */870530776], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[React.createElement("b", undefined, ViewCommon.text(param[0]))])),
                                          ReasonReact.element(/* None */0, /* None */0, MaterialUi_TableCell.make(/* Some */[noBorder], /* None */0, /* Some */[true], /* Some */[/* None */870530776], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[ViewCommon.text(BTC.format(param[1]) + " BTC")]))
                                        ]));
                        }), summary[/* destinations */1]));
              var match$2;
              if (typeof status === "number") {
                switch (status) {
                  case 0 : 
                      match$2 = /* tuple */[
                        "Pending Approval",
                        /* Pending */0
                      ];
                      break;
                  case 1 : 
                      match$2 = /* tuple */[
                        "Accepted",
                        /* Success */2
                      ];
                      break;
                  case 2 : 
                      match$2 = /* tuple */[
                        "Denied",
                        /* Failure */1
                      ];
                      break;
                  case 3 : 
                      match$2 = /* tuple */[
                        "Aborted",
                        /* Failure */1
                      ];
                      break;
                  case 4 : 
                      match$2 = /* tuple */[
                        "Unconfirmed",
                        /* Pending */0
                      ];
                      break;
                  case 5 : 
                      match$2 = /* tuple */[
                        "Confirmed",
                        /* Success */2
                      ];
                      break;
                  
                }
              } else {
                match$2 = /* tuple */[
                  "Failed",
                  /* Failure */1
                ];
              }
              var payoutStatus = ReasonReact.element(/* None */0, /* None */0, StatusChip.make(match$2[1], match$2[0], /* array */[]));
              var transactionId = txId ? React.createElement("div", undefined, ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Title */594052472, /* None */0, /* array */[ViewCommon.text("Transaction ID")])), ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Body2 */-904051920, /* None */0, /* array */[ViewCommon.text(txId[0])]))) : null;
              return ReasonReact.element(/* None */0, /* None */0, Body2.make(/* Some */[/* :: */[
                                "Payout Details",
                                /* [] */0
                              ]], React.createElement("div", undefined, ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Body2 */-904051920, /* None */0, /* array */[date ? ViewCommon.text("Payout completed on " + date[0].toString()) : ViewCommon.text("Proposed by " + PrimitiveTypes.UserId[/* toString */0](match[/* proposedBy */2]))])), ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Body2 */-904051920, /* None */0, /* array */[
                                          ViewCommon.text("Status: "),
                                          payoutStatus
                                        ])), ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Title */594052472, /* None */0, /* array */[ViewCommon.text("Payout")])), ReasonReact.element(/* None */0, /* None */0, MaterialUi_Table.make(/* None */0, /* None */0, /* None */0, /* None */0, /* array */[ReasonReact.element(/* None */0, /* None */0, MaterialUi_TableBody.make(/* None */0, /* None */0, /* array */[
                                                    destinationList,
                                                    ReasonReact.element(/* Some */["networkFee"], /* None */0, MaterialUi_TableRow.make(/* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[
                                                              ReasonReact.element(/* None */0, /* None */0, MaterialUi_TableCell.make(/* Some */[noBorder], /* None */0, /* None */0, /* Some */[/* None */870530776], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[React.createElement("b", undefined, ViewCommon.text("NETWORK FEE"))])),
                                                              ReasonReact.element(/* None */0, /* None */0, MaterialUi_TableCell.make(/* Some */[noBorder], /* None */0, /* Some */[true], /* Some */[/* None */870530776], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[ViewCommon.text(BTC.format(summary[/* networkFee */4]) + " BTC")]))
                                                            ])),
                                                    ReasonReact.element(/* Some */["misthosFee"], /* None */0, MaterialUi_TableRow.make(/* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[
                                                              ReasonReact.element(/* None */0, /* None */0, MaterialUi_TableCell.make(/* Some */[noBorder], /* None */0, /* None */0, /* Some */[/* None */870530776], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[React.createElement("b", undefined, ViewCommon.text("MISTHOS FEE"))])),
                                                              ReasonReact.element(/* None */0, /* None */0, MaterialUi_TableCell.make(/* Some */[noBorder], /* None */0, /* Some */[true], /* Some */[/* None */870530776], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[ViewCommon.text(BTC.format(summary[/* misthosFee */3]) + " BTC")]))
                                                            ]))
                                                  ]))])), React.createElement("div", {
                                      className: total
                                    }, ReasonReact.element(/* None */0, /* None */0, MaterialUi_Typography.make(/* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* Some */[/* Body2 */-904051920], /* None */0, /* None */0, /* array */[ViewCommon.text("TOTAL PAYOUT")])), ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Subheading */148169314, /* Some */[total], /* array */[ViewCommon.text(BTC.format(summary[/* spentWithFees */2]) + " BTC")]))), transactionId), React.createElement("div", undefined, ReasonReact.element(/* None */0, /* None */0, Voters.make(match[/* voters */4], /* array */[])), ReasonReact.element(/* None */0, /* None */0, ProcessApprovalButtons.make("Endorse Payout", "Reject Payout", match[/* canVote */3], (function () {
                                            return Curry._1(commands[/* endorsePayout */8], processId);
                                          }), (function () {
                                            return Curry._1(commands[/* rejectPayout */9], processId);
                                          }), (function () {
                                            return Curry._1(commands[/* reset */0], /* () */0);
                                          }), cmdStatus, /* array */[])), Belt_Set.size(viewData[/* collidesWith */1]) > 0 ? ReasonReact.element(/* None */0, /* None */0, MaterialUi_SnackbarContent.make(/* None */0, /* None */0, /* Some */[ViewCommon.text("\n                   This Proposal is reusing inputs reserved by another payout.\n                   We recommend that you coordinate with your Partners\n                   to only endorse one Proposal and reject the other one.\n                   ")], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[])) : null), /* array */[]));
            }),
          /* initialState */component[/* initialState */10],
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */component[/* reducer */12],
          /* subscriptions */component[/* subscriptions */13],
          /* jsElementWrapped */component[/* jsElementWrapped */14]
        ];
}

var text = ViewCommon.text;

var extractString = ViewCommon.extractString;

var ViewData = 0;

exports.text = text;
exports.extractString = extractString;
exports.ViewData = ViewData;
exports.component = component;
exports.Styles = Styles;
exports.make = make;
/* component Not a pure module */
