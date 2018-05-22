// Generated by BUCKLESCRIPT VERSION 3.0.0, PLEASE EDIT WITH CARE
'use strict';

var BTC = require("../application/wallet/BTC.bs.js");
var Css = require("bs-css/src/Css.js");
var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Block = require("bs-platform/lib/js/block.js");
var Body2 = require("./components/Body2.bs.js");
var Curry = require("bs-platform/lib/js/curry.js");
var React = require("react");
var MInput = require("./components/MInput.bs.js");
var Balance = require("./components/Balance.bs.js");
var MButton = require("./components/MButton.bs.js");
var Spinner = require("./components/Spinner.bs.js");
var MaterialUi = require("@jsiebern/bs-material-ui/src/MaterialUi.bs.js");
var ViewCommon = require("./ViewCommon.bs.js");
var MTypography = require("./components/MTypography.bs.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");
var WalletTypes = require("../application/wallet/WalletTypes.bs.js");
var RemoveSvg = require("../assets/img/remove.svg");

var defaultFee = BTC.fromSatoshis(/* int64 */[
      /* hi */0,
      /* lo */100
    ]);

var component = ReasonReact.reducerComponent("CreatePayout");

var max = Css.style(/* [] */0);

var Styles = /* module */[/* max */max];

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
          /* render */(function (param) {
              var send = param[/* send */3];
              var match = param[/* state */1];
              var inputs = match[/* inputs */5];
              var summary = match[/* summary */4];
              var viewData = match[/* viewData */0];
              var spinner;
              spinner = typeof cmdStatus === "number" ? null : (
                  cmdStatus.tag ? ViewCommon.text("done") : ReasonReact.element(/* None */0, /* None */0, Spinner.make("waiting for result", /* None */0, /* array */[]))
                );
              var destinationList = $$Array.of_list(List.mapi((function (idx, param) {
                          return ReasonReact.element(/* Some */[String(idx)], /* None */0, MaterialUi.TableRow[/* make */1](/* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[
                                          ReasonReact.element(/* None */0, /* None */0, MaterialUi.TableCell[/* make */7](/* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[React.createElement("b", undefined, ViewCommon.text(param[0]))])),
                                          ReasonReact.element(/* None */0, /* None */0, MaterialUi.TableCell[/* make */7](/* None */0, /* None */0, /* Some */[true], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[ViewCommon.text(BTC.format(param[1]) + " BTC")])),
                                          ReasonReact.element(/* None */0, /* None */0, MaterialUi.TableCell[/* make */7](/* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[ReasonReact.element(/* None */0, /* None */0, MaterialUi.IconButton[/* make */3](/* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* Some */[(function () {
                                                                  console.log("TODO");
                                                                  return /* () */0;
                                                                })], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[React.createElement("img", {
                                                                    alt: "Remove",
                                                                    src: RemoveSvg
                                                                  })]))]))
                                        ]));
                        }), match[/* destinations */1]));
              return ReasonReact.element(/* None */0, /* None */0, Body2.make(/* Some */[/* :: */[
                                "Create A Payout",
                                /* [] */0
                              ]], React.createElement("div", undefined, ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Title */594052472, /* None */0, /* array */[ViewCommon.text(viewData[/* ventureName */1])])), ReasonReact.element(/* None */0, /* None */0, Balance.make(viewData[/* balance */2], /* None */0, /* array */[])), ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Title */594052472, /* None */0, /* array */[ViewCommon.text("Proposed recipients")])), ReasonReact.element(/* None */0, /* None */0, MaterialUi.Table[/* make */1](/* None */0, /* None */0, /* None */0, /* array */[ReasonReact.element(/* None */0, /* None */0, MaterialUi.TableBody[/* make */0](/* None */0, /* None */0, /* array */[
                                                    destinationList,
                                                    ReasonReact.element(/* Some */["networkFee"], /* None */0, MaterialUi.TableRow[/* make */1](/* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[
                                                              ReasonReact.element(/* None */0, /* None */0, MaterialUi.TableCell[/* make */7](/* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[React.createElement("b", undefined, ViewCommon.text("NETWORK FEE"))])),
                                                              ReasonReact.element(/* None */0, /* None */0, MaterialUi.TableCell[/* make */7](/* None */0, /* None */0, /* Some */[true], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[ViewCommon.text(BTC.format(summary[/* networkFee */4]) + " BTC")])),
                                                              ReasonReact.element(/* None */0, /* None */0, MaterialUi.TableCell[/* make */7](/* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[]))
                                                            ])),
                                                    ReasonReact.element(/* Some */["misthosFee"], /* None */0, MaterialUi.TableRow[/* make */1](/* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[
                                                              ReasonReact.element(/* None */0, /* None */0, MaterialUi.TableCell[/* make */7](/* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[React.createElement("b", undefined, ViewCommon.text("MISTHOS FEE"))])),
                                                              ReasonReact.element(/* None */0, /* None */0, MaterialUi.TableCell[/* make */7](/* None */0, /* None */0, /* Some */[true], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[ViewCommon.text(BTC.format(summary[/* misthosFee */3]) + " BTC")])),
                                                              ReasonReact.element(/* None */0, /* None */0, MaterialUi.TableCell[/* make */7](/* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[]))
                                                            ]))
                                                  ]))]))), React.createElement("div", undefined, ReasonReact.element(/* None */0, /* None */0, MInput.make(/* Some */["Recipient Address"], /* Some */[/* `String */[
                                            -976970511,
                                            inputs[/* recipientAddress */0]
                                          ]], /* Some */[(function (e) {
                                              return Curry._1(send, /* ChangeRecipientAddress */Block.__(0, [ViewCommon.extractString(e)]));
                                            })], /* Some */[false], /* Some */[true], /* None */0, /* None */0, /* array */[])), ReasonReact.element(/* None */0, /* None */0, MInput.make(/* Some */["BTC amount"], /* Some */[/* `String */[
                                            -976970511,
                                            inputs[/* btcAmount */1]
                                          ]], /* Some */[(function (e) {
                                              return Curry._1(send, /* ChangeBTCAmount */Block.__(1, [ViewCommon.extractString(e)]));
                                            })], /* Some */[false], /* Some */[true], /* Some */[ReasonReact.element(/* None */0, /* None */0, MaterialUi.InputAdornment[/* make */3](/* None */0, /* None */0, /* None */0, /* Some */[/* End */3455931], /* None */0, /* array */[ReasonReact.element(/* None */0, /* None */0, MButton.make(/* None */0, /* Some */[(function () {
                                                                  return Curry._1(send, /* EnterMax */0);
                                                                })], /* Some */[/* Small */311976103], /* None */0, /* Some */[/* Flat */0], /* array */[ViewCommon.text("Max")]))]))], /* Some */[true], /* array */[])), ReasonReact.element(/* None */0, /* None */0, MButton.make(/* None */0, /* Some */[(function () {
                                              return Curry._1(send, /* AddAnother */1);
                                            })], /* None */0, /* None */0, /* Some */[/* Flat */0], /* array */[ViewCommon.text("+ add another recipient")])), ReasonReact.element(/* None */0, /* None */0, MButton.make(/* None */0, /* Some */[(function () {
                                              return Curry._1(send, /* ProposePayout */2);
                                            })], /* None */0, /* Some */[true], /* None */0, /* array */[ViewCommon.text("Propose Payout")])), spinner), /* array */[]));
            }),
          /* initialState */(function () {
              return /* record */[
                      /* viewData */viewData,
                      /* destinations : [] */0,
                      /* inputDestination */"",
                      /* inputAmount */BTC.zero,
                      /* summary */viewData[/* initialSummary */3],
                      /* inputs : record */[
                        /* recipientAddress */"",
                        /* btcAmount */""
                      ]
                    ];
            }),
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */(function (action, state) {
              var viewData = state[/* viewData */0];
              if (typeof action === "number") {
                switch (action) {
                  case 0 : 
                      var max = Curry._3(viewData[/* max */5], state[/* inputDestination */2], state[/* destinations */1], defaultFee);
                      return /* SideEffects */Block.__(1, [(function (param) {
                                    return Curry._1(param[/* send */3], /* ChangeBTCAmount */Block.__(1, [BTC.format(max)]));
                                  })]);
                  case 1 : 
                      if (state[/* inputDestination */2] !== "" && state[/* inputAmount */3].gt(BTC.zero)) {
                        return /* Update */Block.__(0, [/* record */[
                                    /* viewData */state[/* viewData */0],
                                    /* destinations : :: */[
                                      /* tuple */[
                                        state[/* inputDestination */2],
                                        state[/* inputAmount */3]
                                      ],
                                      state[/* destinations */1]
                                    ],
                                    /* inputDestination */"",
                                    /* inputAmount */BTC.zero,
                                    /* summary */state[/* summary */4],
                                    /* inputs : record */[
                                      /* recipientAddress */"",
                                      /* btcAmount */""
                                    ]
                                  ]]);
                      } else {
                        return /* NoUpdate */0;
                      }
                  case 2 : 
                      var destinations = state[/* inputDestination */2] !== "" && state[/* inputAmount */3].gt(BTC.zero) ? /* :: */[
                          /* tuple */[
                            state[/* inputDestination */2],
                            state[/* inputAmount */3]
                          ],
                          state[/* destinations */1]
                        ] : state[/* destinations */1];
                      Curry._3(commands[/* proposePayout */0], WalletTypes.AccountIndex[/* default */9], destinations, defaultFee);
                      return /* NoUpdate */0;
                  
                }
              } else if (action.tag) {
                var amount = action[0];
                var inputAmount = BTC.fromString(amount);
                var match = inputAmount.isNaN();
                var match$1 = match ? /* tuple */[
                    state[/* inputAmount */3],
                    state[/* inputs */5][/* btcAmount */1]
                  ] : /* tuple */[
                    inputAmount,
                    amount
                  ];
                var btcAmount = match$1[1];
                var inputAmount$1 = match$1[0];
                var match$2;
                if (state[/* inputDestination */2] !== "") {
                  var max$1 = Curry._3(viewData[/* max */5], state[/* inputDestination */2], state[/* destinations */1], defaultFee);
                  var match$3 = inputAmount$1.gt(max$1);
                  var match$4 = match$3 ? /* tuple */[
                      max$1,
                      BTC.format(max$1)
                    ] : /* tuple */[
                      inputAmount$1,
                      btcAmount
                    ];
                  var inputAmount$2 = match$4[0];
                  match$2 = /* tuple */[
                    Curry._2(viewData[/* summary */6], /* :: */[
                          /* tuple */[
                            state[/* inputDestination */2],
                            inputAmount$2
                          ],
                          state[/* destinations */1]
                        ], defaultFee),
                    inputAmount$2,
                    match$4[1]
                  ];
                } else {
                  match$2 = /* tuple */[
                    state[/* summary */4],
                    inputAmount$1,
                    btcAmount
                  ];
                }
                var init = state[/* inputs */5];
                return /* Update */Block.__(0, [/* record */[
                            /* viewData */state[/* viewData */0],
                            /* destinations */state[/* destinations */1],
                            /* inputDestination */state[/* inputDestination */2],
                            /* inputAmount */match$2[1],
                            /* summary */match$2[0],
                            /* inputs : record */[
                              /* recipientAddress */init[/* recipientAddress */0],
                              /* btcAmount */match$2[2]
                            ]
                          ]]);
              } else {
                var address = action[0];
                var match$5;
                if (Curry._1(viewData[/* isAddressValid */4], address)) {
                  var max$2 = Curry._3(viewData[/* max */5], address, state[/* destinations */1], defaultFee);
                  var match$6 = state[/* inputAmount */3].gt(max$2);
                  var match$7 = match$6 ? /* tuple */[
                      max$2,
                      BTC.format(max$2)
                    ] : /* tuple */[
                      state[/* inputAmount */3],
                      state[/* inputs */5][/* btcAmount */1]
                    ];
                  var inputAmount$3 = match$7[0];
                  match$5 = /* tuple */[
                    Curry._2(viewData[/* summary */6], /* :: */[
                          /* tuple */[
                            address,
                            inputAmount$3
                          ],
                          state[/* destinations */1]
                        ], defaultFee),
                    address,
                    inputAmount$3,
                    match$7[1]
                  ];
                } else {
                  match$5 = /* tuple */[
                    state[/* summary */4],
                    "",
                    state[/* inputAmount */3],
                    state[/* inputs */5][/* btcAmount */1]
                  ];
                }
                return /* Update */Block.__(0, [/* record */[
                            /* viewData */state[/* viewData */0],
                            /* destinations */state[/* destinations */1],
                            /* inputDestination */match$5[1],
                            /* inputAmount */match$5[2],
                            /* summary */match$5[0],
                            /* inputs : record */[
                              /* recipientAddress */address,
                              /* btcAmount */match$5[3]
                            ]
                          ]]);
              }
            }),
          /* subscriptions */component[/* subscriptions */13],
          /* jsElementWrapped */component[/* jsElementWrapped */14]
        ];
}

var text = ViewCommon.text;

var extractString = ViewCommon.extractString;

var View = 0;

exports.text = text;
exports.extractString = extractString;
exports.View = View;
exports.defaultFee = defaultFee;
exports.component = component;
exports.Styles = Styles;
exports.make = make;
/* defaultFee Not a pure module */