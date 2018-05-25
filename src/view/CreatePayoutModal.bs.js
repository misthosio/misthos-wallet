// Generated by BUCKLESCRIPT VERSION 3.1.4, PLEASE EDIT WITH CARE
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
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var ViewCommon = require("./ViewCommon.bs.js");
var MTypography = require("./components/MTypography.bs.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");
var WalletTypes = require("../application/wallet/WalletTypes.bs.js");
var MaterialUi_Table = require("@jsiebern/bs-material-ui/src/MaterialUi_Table.bs.js");
var MaterialUi_TableRow = require("@jsiebern/bs-material-ui/src/MaterialUi_TableRow.bs.js");
var MaterialUi_TableBody = require("@jsiebern/bs-material-ui/src/MaterialUi_TableBody.bs.js");
var MaterialUi_TableCell = require("@jsiebern/bs-material-ui/src/MaterialUi_TableCell.bs.js");
var MaterialUi_IconButton = require("@jsiebern/bs-material-ui/src/MaterialUi_IconButton.bs.js");
var MaterialUi_Typography = require("@jsiebern/bs-material-ui/src/MaterialUi_Typography.bs.js");
var RemoveSvg = require("../assets/img/remove.svg");
var MaterialUi_InputAdornment = require("@jsiebern/bs-material-ui/src/MaterialUi_InputAdornment.bs.js");

var defaultFee = BTC.fromSatoshis(/* int64 */[
      /* hi */0,
      /* lo */100
    ]);

var component = ReasonReact.reducerComponent("CreatePayout");

var maxButton = Css.style(/* :: */[
      Css.color(Css.rgba(0, 0, 0, 0.54)),
      /* [] */0
    ]);

var maxWidth = Css.style(/* :: */[
      Css.width(/* `percent */[
            -119887163,
            99.0
          ]),
      /* [] */0
    ]);

var buttonPadding = Css.style(/* :: */[
      Css.paddingLeft(Css.px(4)),
      /* [] */0
    ]);

var noBorder = Css.style(/* :: */[
      Css.borderColor(/* transparent */582626130),
      /* :: */[
        Css.whiteSpace(/* nowrap */867913355),
        /* [] */0
      ]
    ]);

function spaceBetween(align) {
  return Css.style(/* :: */[
              Css.display(/* flex */-1010954439),
              /* :: */[
                Css.justifyContent(/* spaceBetween */516682146),
                /* :: */[
                  Css.alignItems(align),
                  /* [] */0
                ]
              ]
            ]);
}

var Styles = /* module */[
  /* maxButton */maxButton,
  /* maxWidth */maxWidth,
  /* buttonPadding */buttonPadding,
  /* noBorder */noBorder,
  /* spaceBetween */spaceBetween
];

function updateState(state) {
  var match = state[/* inputs */6];
  var btcAmount = match[/* btcAmount */1];
  var recipientAddress = match[/* recipientAddress */0];
  var inputAmount = state[/* inputAmount */3];
  var destinations = state[/* destinations */1];
  var viewData = state[/* viewData */0];
  var match$1 = Curry._1(viewData[/* isAddressValid */5], recipientAddress) ? /* tuple */[
      recipientAddress,
      recipientAddress,
      true
    ] : /* tuple */[
      recipientAddress,
      "",
      false
    ];
  var addressValid = match$1[2];
  var inputDestination = match$1[1];
  var recipientAddress$1 = match$1[0];
  var newInputAmount = BTC.fromString(btcAmount);
  var match$2 = btcAmount === "" ? /* tuple */[
      "",
      BTC.zero
    ] : (
      newInputAmount.isNaN() ? /* tuple */[
          BTC.format(inputAmount),
          inputAmount
        ] : /* tuple */[
          btcAmount,
          newInputAmount
        ]
    );
  var inputAmount$1 = match$2[1];
  var btcAmount$1 = match$2[0];
  if (inputAmount$1.gt(BTC.zero) && inputDestination !== "") {
    var max = Curry._3(viewData[/* max */6], inputDestination, destinations, defaultFee);
    var match$3 = inputAmount$1.gt(max);
    var match$4 = match$3 ? /* tuple */[
        max,
        BTC.format(max)
      ] : /* tuple */[
        inputAmount$1,
        btcAmount$1
      ];
    var inputAmount$2 = match$4[0];
    return /* record */[
            /* viewData */viewData,
            /* destinations */state[/* destinations */1],
            /* inputDestination */inputDestination,
            /* inputAmount */inputAmount$2,
            /* addressValid */addressValid,
            /* summary */Curry._2(viewData[/* summary */7], /* :: */[
                  /* tuple */[
                    inputDestination,
                    inputAmount$2
                  ],
                  destinations
                ], defaultFee),
            /* inputs : record */[
              /* recipientAddress */recipientAddress$1,
              /* btcAmount */match$4[1]
            ]
          ];
  } else {
    return /* record */[
            /* viewData */viewData,
            /* destinations */state[/* destinations */1],
            /* inputDestination */inputDestination,
            /* inputAmount */inputAmount$1,
            /* addressValid */addressValid,
            /* summary */Curry._2(viewData[/* summary */7], destinations, defaultFee),
            /* inputs : record */[
              /* recipientAddress */recipientAddress$1,
              /* btcAmount */btcAmount$1
            ]
          ];
  }
}

function make(viewData, commands, cmdStatus, _) {
  return /* record */[
          /* debugName */component[/* debugName */0],
          /* reactClassInternal */component[/* reactClassInternal */1],
          /* handedOffState */component[/* handedOffState */2],
          /* willReceiveProps */(function (param) {
              var state = param[/* state */1];
              return /* record */[
                      /* viewData */viewData,
                      /* destinations */state[/* destinations */1],
                      /* inputDestination */state[/* inputDestination */2],
                      /* inputAmount */state[/* inputAmount */3],
                      /* addressValid */state[/* addressValid */4],
                      /* summary */state[/* summary */5],
                      /* inputs */state[/* inputs */6]
                    ];
            }),
          /* didMount */component[/* didMount */4],
          /* didUpdate */component[/* didUpdate */5],
          /* willUnmount */component[/* willUnmount */6],
          /* willUpdate */component[/* willUpdate */7],
          /* shouldUpdate */component[/* shouldUpdate */8],
          /* render */(function (param) {
              var send = param[/* send */3];
              var match = param[/* state */1];
              var inputs = match[/* inputs */6];
              var summary = match[/* summary */5];
              var viewData = match[/* viewData */0];
              var feedback;
              if (typeof cmdStatus === "number") {
                feedback = null;
              } else {
                switch (cmdStatus.tag | 0) {
                  case 0 : 
                      feedback = ReasonReact.element(/* None */0, /* None */0, Spinner.make("waiting for result", /* None */0, /* array */[]));
                      break;
                  case 1 : 
                      feedback = ViewCommon.text("Could not execute teh command");
                      break;
                  case 2 : 
                      feedback = null;
                      break;
                  
                }
              }
              var destinationList = $$Array.of_list(List.mapi((function (idx, param) {
                          return ReasonReact.element(/* Some */[String(idx)], /* None */0, MaterialUi_TableRow.make(/* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[
                                          ReasonReact.element(/* None */0, /* None */0, MaterialUi_TableCell.make(/* Some */[spaceBetween(/* center */98248149) + (" " + noBorder)], /* None */0, /* None */0, /* Some */[/* None */870530776], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[
                                                    React.createElement("b", undefined, ViewCommon.text(param[0])),
                                                    ReasonReact.element(/* None */0, /* None */0, MaterialUi_IconButton.make(/* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* Some */[(function () {
                                                                  return Curry._1(send, /* RemoveDestination */Block.__(2, [idx]));
                                                                })], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[React.createElement("img", {
                                                                    alt: "Remove",
                                                                    src: RemoveSvg
                                                                  })]))
                                                  ])),
                                          ReasonReact.element(/* None */0, /* None */0, MaterialUi_TableCell.make(/* Some */[noBorder], /* None */0, /* Some */[true], /* Some */[/* None */870530776], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[ViewCommon.text(BTC.format(param[1]) + " BTC")]))
                                        ]));
                        }), match[/* destinations */1]));
              var tmp;
              if (viewData[/* allowCreation */0] === false) {
                tmp = React.createElement("div", undefined, ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Title */594052472, /* None */0, /* array */[ViewCommon.text(viewData[/* ventureName */3])])), ReasonReact.element(/* None */0, /* None */0, Balance.make(viewData[/* balance */1][/* currentSpendable */0], /* Some */[viewData[/* balance */1][/* reserved */1]], /* array */[])));
              } else {
                var error = match[/* addressValid */4] ? /* None */0 : /* Some */["Address is BAD"];
                tmp = React.createElement("div", undefined, ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Title */594052472, /* None */0, /* array */[ViewCommon.text(viewData[/* ventureName */3])])), ReasonReact.element(/* None */0, /* None */0, Balance.make(viewData[/* balance */1][/* currentSpendable */0], /* Some */[viewData[/* balance */1][/* reserved */1]], /* array */[])), ReasonReact.element(/* None */0, /* None */0, MInput.make(/* Some */["Recipient Address"], /* Some */[/* `String */[
                                -976970511,
                                inputs[/* recipientAddress */0]
                              ]], /* Some */[(function (e) {
                                  return Curry._1(send, /* ChangeRecipientAddress */Block.__(0, [ViewCommon.extractString(e)]));
                                })], /* Some */[false], /* Some */[true], /* None */0, error, /* None */0, /* array */[])), ReasonReact.element(/* None */0, /* None */0, MInput.make(/* Some */["BTC amount"], /* Some */[/* `String */[
                                -976970511,
                                inputs[/* btcAmount */1]
                              ]], /* Some */[(function (e) {
                                  return Curry._1(send, /* ChangeBTCAmount */Block.__(1, [ViewCommon.extractString(e)]));
                                })], /* Some */[false], /* Some */[true], /* Some */[ReasonReact.element(/* None */0, /* None */0, MaterialUi_InputAdornment.make(/* None */0, /* None */0, /* None */0, /* Some */[/* End */3455931], /* None */0, /* None */0, /* array */[ReasonReact.element(/* None */0, /* None */0, MButton.make(/* None */0, /* Some */[(function () {
                                                      return Curry._1(send, /* EnterMax */0);
                                                    })], /* Some */[/* Small */311976103], /* None */0, /* Some */[/* Flat */0], /* Some */[maxButton], /* array */[ViewCommon.text("Max")]))]))], /* None */0, /* Some */[true], /* array */[])), ReasonReact.element(/* None */0, /* None */0, MButton.make(/* None */0, /* Some */[(function () {
                                  return Curry._1(send, /* AddToSummary */1);
                                })], /* None */0, /* Some */[true], /* None */0, /* None */0, /* array */[ViewCommon.text("Add to Summary")])));
              }
              return ReasonReact.element(/* None */0, /* None */0, Body2.make(/* Some */[/* :: */[
                                "Create A Payout",
                                /* [] */0
                              ]], tmp, viewData[/* allowCreation */0] === false ? React.createElement("div", undefined, ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Body2 */-904051920, /* None */0, /* array */[ViewCommon.text("Cannot create Payout without unreserved balance")]))) : React.createElement("div", undefined, ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Title */594052472, /* None */0, /* array */[ViewCommon.text("Summary")])), ReasonReact.element(/* None */0, /* None */0, MaterialUi_Table.make(/* None */0, /* None */0, /* None */0, /* None */0, /* array */[ReasonReact.element(/* None */0, /* None */0, MaterialUi_TableBody.make(/* None */0, /* None */0, /* array */[
                                                      destinationList,
                                                      ReasonReact.element(/* Some */["networkFee"], /* None */0, MaterialUi_TableRow.make(/* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[
                                                                ReasonReact.element(/* None */0, /* None */0, MaterialUi_TableCell.make(/* Some */[noBorder], /* None */0, /* None */0, /* Some */[/* None */870530776], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[React.createElement("b", undefined, ViewCommon.text("NETWORK FEE"))])),
                                                                ReasonReact.element(/* None */0, /* None */0, MaterialUi_TableCell.make(/* Some */[maxWidth + (" " + noBorder)], /* None */0, /* Some */[true], /* Some */[/* None */870530776], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[ViewCommon.text(BTC.format(summary[/* networkFee */4]) + " BTC")]))
                                                              ])),
                                                      ReasonReact.element(/* Some */["misthosFee"], /* None */0, MaterialUi_TableRow.make(/* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[
                                                                ReasonReact.element(/* None */0, /* None */0, MaterialUi_TableCell.make(/* Some */[noBorder], /* None */0, /* None */0, /* Some */[/* None */870530776], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[React.createElement("b", undefined, ViewCommon.text("MISTHOS FEE"))])),
                                                                ReasonReact.element(/* None */0, /* None */0, MaterialUi_TableCell.make(/* Some */[noBorder], /* None */0, /* Some */[true], /* Some */[/* None */870530776], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[ViewCommon.text(BTC.format(summary[/* misthosFee */3]) + " BTC")]))
                                                              ]))
                                                    ]))])), React.createElement("div", {
                                        className: spaceBetween(/* baseline */287825029)
                                      }, ReasonReact.element(/* None */0, /* None */0, MaterialUi_Typography.make(/* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* Some */[/* Body2 */-904051920], /* None */0, /* None */0, /* array */[ViewCommon.text("TOTAL PAYOUT")])), ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Subheading */148169314, /* None */0, /* array */[ViewCommon.text(BTC.format(summary[/* spentWithFees */2]) + " BTC")]))), ReasonReact.element(/* None */0, /* None */0, MButton.make(/* None */0, /* Some */[(function () {
                                                return Curry._1(send, /* ProposePayout */2);
                                              })], /* None */0, /* Some */[true], /* None */0, /* None */0, /* array */[ViewCommon.text("Propose Payout")])), feedback), /* array */[]));
            }),
          /* initialState */(function () {
              return /* record */[
                      /* viewData */viewData,
                      /* destinations : [] */0,
                      /* inputDestination */"",
                      /* inputAmount */BTC.zero,
                      /* addressValid */true,
                      /* summary */viewData[/* initialSummary */4],
                      /* inputs : record */[
                        /* recipientAddress */"",
                        /* btcAmount */""
                      ]
                    ];
            }),
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */(function (action, state) {
              if (typeof action === "number") {
                switch (action) {
                  case 0 : 
                      var max = Curry._3(state[/* viewData */0][/* max */6], state[/* inputDestination */2], state[/* destinations */1], defaultFee);
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
                                    /* addressValid */state[/* addressValid */4],
                                    /* summary */state[/* summary */5],
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
                      Curry._3(commands[/* proposePayout */7], WalletTypes.AccountIndex[/* default */9], destinations, defaultFee);
                      return /* NoUpdate */0;
                  
                }
              } else {
                switch (action.tag | 0) {
                  case 0 : 
                      var init = state[/* inputs */6];
                      return /* Update */Block.__(0, [updateState(/* record */[
                                      /* viewData */state[/* viewData */0],
                                      /* destinations */state[/* destinations */1],
                                      /* inputDestination */state[/* inputDestination */2],
                                      /* inputAmount */state[/* inputAmount */3],
                                      /* addressValid */state[/* addressValid */4],
                                      /* summary */state[/* summary */5],
                                      /* inputs : record */[
                                        /* recipientAddress */action[0],
                                        /* btcAmount */init[/* btcAmount */1]
                                      ]
                                    ])]);
                  case 1 : 
                      var init$1 = state[/* inputs */6];
                      return /* Update */Block.__(0, [updateState(/* record */[
                                      /* viewData */state[/* viewData */0],
                                      /* destinations */state[/* destinations */1],
                                      /* inputDestination */state[/* inputDestination */2],
                                      /* inputAmount */state[/* inputAmount */3],
                                      /* addressValid */state[/* addressValid */4],
                                      /* summary */state[/* summary */5],
                                      /* inputs : record */[
                                        /* recipientAddress */init$1[/* recipientAddress */0],
                                        /* btcAmount */action[0]
                                      ]
                                    ])]);
                  case 2 : 
                      var removeIdx = action[0];
                      return /* Update */Block.__(0, [updateState(/* record */[
                                      /* viewData */state[/* viewData */0],
                                      /* destinations */Belt_List.keepMapU(Belt_List.mapWithIndexU(state[/* destinations */1], (function (idx, destination) {
                                                  var match = idx === removeIdx;
                                                  if (match) {
                                                    return /* None */0;
                                                  } else {
                                                    return /* Some */[destination];
                                                  }
                                                })), (function (d) {
                                              return d;
                                            })),
                                      /* inputDestination */state[/* inputDestination */2],
                                      /* inputAmount */state[/* inputAmount */3],
                                      /* addressValid */state[/* addressValid */4],
                                      /* summary */state[/* summary */5],
                                      /* inputs */state[/* inputs */6]
                                    ])]);
                  
                }
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
exports.updateState = updateState;
exports.make = make;
/* defaultFee Not a pure module */
