// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE
'use strict';

var Css = require("bs-css/src/Css.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var React = require("react");
var Colors = require("../Colors.bs.js");
var MButton = require("./MButton.bs.js");
var Js_option = require("bs-platform/lib/js/js_option.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var ViewCommon = require("../ViewCommon.bs.js");
var MTypography = require("./MTypography.bs.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");
var CommandExecutor = require("./CommandExecutor.bs.js");

var component = ReasonReact.reducerComponent("ProcessApprovalButtons");

var gray = Css.style(/* :: */[
      Css.color(Css.rgba(0, 0, 0, 0.38)),
      /* [] */0
    ]);

var inlineConfirm = Css.style(/* :: */[
      Css.display(/* flex */-1010954439),
      /* :: */[
        Css.alignItems(/* baseline */287825029),
        /* [] */0
      ]
    ]);

var warning = Css.style(/* :: */[
      Css.color(Colors.error),
      /* [] */0
    ]);

var Styles = /* module */[
  /* gray */gray,
  /* inlineConfirm */inlineConfirm,
  /* warning */warning
];

function make(endorseText, alertText, rejectText, canVote, onEndorse, onReject, onCancel, cmdStatus, _) {
  return /* record */[
          /* debugName */component[/* debugName */0],
          /* reactClassInternal */component[/* reactClassInternal */1],
          /* handedOffState */component[/* handedOffState */2],
          /* willReceiveProps */(function (param) {
              return /* record */[
                      /* buttonState */param[/* state */1][/* buttonState */0],
                      /* cmdStatus */cmdStatus
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
              var cmdStatus = match[/* cmdStatus */1];
              var state = match[/* buttonState */0];
              var tmp;
              var exit = 0;
              var exit$1 = 0;
              switch (state) {
                case 0 : 
                    if (canVote) {
                      exit = 2;
                    } else {
                      tmp = /* array */[null];
                    }
                    break;
                case 1 : 
                    tmp = /* array */[
                      ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Body2 */-904051920, /* Some */[warning], /* None */0, /* None */0, /* None */0, /* None */0, /* array */[ViewCommon.text(Js_option.getWithDefault("", alertText))])),
                      ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Body2 */-904051920, /* Some */[inlineConfirm], /* None */0, /* None */0, /* None */0, /* None */0, /* array */[
                                ViewCommon.text(endorseText),
                                ReasonReact.element(/* None */0, /* None */0, MButton.make(/* None */0, /* Some */[(function () {
                                              return Curry._1(send, /* ConfirmEndorse */2);
                                            })], /* None */0, /* None */0, /* Some */[/* Flat */0], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[ViewCommon.text("yes")])),
                                ReasonReact.element(/* None */0, /* None */0, MButton.make(/* None */0, /* Some */[(function () {
                                              return Curry._1(send, /* Cancel */0);
                                            })], /* None */0, /* None */0, /* Some */[/* Flat */0], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[ViewCommon.text("No")]))
                              ]))
                    ];
                    break;
                case 3 : 
                    tmp = /* array */[ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Body2 */-904051920, /* Some */[inlineConfirm], /* None */0, /* None */0, /* None */0, /* None */0, /* array */[
                                ViewCommon.text(rejectText),
                                ReasonReact.element(/* None */0, /* None */0, MButton.make(/* None */0, /* Some */[(function () {
                                              return Curry._1(send, /* ConfirmReject */4);
                                            })], /* None */0, /* None */0, /* Some */[/* Flat */0], /* None */0, /* Some */[false], /* None */0, /* None */0, /* None */0, /* array */[ViewCommon.text("yes")])),
                                ReasonReact.element(/* None */0, /* None */0, MButton.make(/* None */0, /* Some */[(function () {
                                              return Curry._1(send, /* Cancel */0);
                                            })], /* None */0, /* None */0, /* Some */[/* Flat */0], /* None */0, /* Some */[false], /* None */0, /* None */0, /* None */0, /* array */[ViewCommon.text("No")]))
                              ]))];
                    break;
                case 2 : 
                case 4 : 
                    exit$1 = 3;
                    break;
                
              }
              if (exit$1 === 3) {
                if (typeof cmdStatus === "number") {
                  exit = canVote ? 2 : 1;
                } else {
                  switch (cmdStatus.tag | 0) {
                    case 1 : 
                        if (state >= 4) {
                          tmp = /* array */[
                            ReasonReact.element(/* None */0, /* None */0, CommandExecutor.Status[/* make */2](cmdStatus, /* Endorsement */4, /* array */[])),
                            ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Body2 */-904051920, /* Some */[inlineConfirm], /* None */0, /* None */0, /* None */0, /* None */0, /* array */[ReasonReact.element(/* None */0, /* None */0, MButton.make(/* None */0, /* Some */[(function () {
                                                    return Curry._1(send, /* Cancel */0);
                                                  })], /* None */0, /* None */0, /* Some */[/* Flat */0], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[ViewCommon.text("Try Again")]))]))
                          ];
                        } else if (cmdStatus[0] >= 6) {
                          tmp = /* array */[
                            ReasonReact.element(/* None */0, /* None */0, CommandExecutor.Status[/* make */2](cmdStatus, /* Rejection */5, /* array */[])),
                            ReasonReact.element(/* None */0, /* None */0, MButton.make(/* None */0, /* Some */[(function () {
                                          return Curry._1(send, /* Cancel */0);
                                        })], /* None */0, /* None */0, /* Some */[/* Flat */0], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[ViewCommon.text("Try Again")]))
                          ];
                        } else {
                          exit = 1;
                        }
                        break;
                    case 0 : 
                    case 2 : 
                        exit = 1;
                        break;
                    
                  }
                }
              }
              switch (exit) {
                case 1 : 
                    tmp = state >= 4 ? /* array */[ReasonReact.element(/* None */0, /* None */0, CommandExecutor.Status[/* make */2](cmdStatus, /* Rejection */5, /* array */[]))] : /* array */[ReasonReact.element(/* None */0, /* None */0, CommandExecutor.Status[/* make */2](cmdStatus, /* Endorsement */4, /* array */[]))];
                    break;
                case 2 : 
                    tmp = /* array */[
                      ReasonReact.element(/* None */0, /* None */0, MButton.make(/* None */0, /* Some */[(function () {
                                    return Curry._1(send, /* Endorse */1);
                                  })], /* None */0, /* Some */[true], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[ViewCommon.text(endorseText)])),
                      ReasonReact.element(/* None */0, /* None */0, MButton.make(/* None */0, /* Some */[(function () {
                                    return Curry._1(send, /* Reject */3);
                                  })], /* None */0, /* None */0, /* Some */[/* Flat */0], /* Some */[gray], /* None */0, /* None */0, /* None */0, /* None */0, /* array */[ViewCommon.text(rejectText)]))
                    ];
                    break;
                
              }
              return React.createElement("div", undefined, Belt_Array.concatMany(/* array */[tmp]));
            }),
          /* initialState */(function () {
              return /* record */[
                      /* buttonState : NoDecision */0,
                      /* cmdStatus : Idle */0
                    ];
            }),
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */(function (action, state) {
              switch (action) {
                case 0 : 
                    return /* UpdateWithSideEffects */Block.__(2, [
                              /* record */[
                                /* buttonState : NoDecision */0,
                                /* cmdStatus : Idle */0
                              ],
                              (function () {
                                  return Curry._1(onCancel, /* () */0);
                                })
                            ]);
                case 1 : 
                    return /* Update */Block.__(0, [/* record */[
                                /* buttonState : ConfirmEndorse */1,
                                /* cmdStatus */state[/* cmdStatus */1]
                              ]]);
                case 2 : 
                    return /* UpdateWithSideEffects */Block.__(2, [
                              /* record */[
                                /* buttonState : EndorsementSubmited */2,
                                /* cmdStatus */state[/* cmdStatus */1]
                              ],
                              (function () {
                                  return Curry._1(onEndorse, /* () */0);
                                })
                            ]);
                case 3 : 
                    return /* Update */Block.__(0, [/* record */[
                                /* buttonState : ConfirmReject */3,
                                /* cmdStatus */state[/* cmdStatus */1]
                              ]]);
                case 4 : 
                    return /* UpdateWithSideEffects */Block.__(2, [
                              /* record */[
                                /* buttonState : RejectionSubmited */4,
                                /* cmdStatus */state[/* cmdStatus */1]
                              ],
                              (function () {
                                  return Curry._1(onReject, /* () */0);
                                })
                            ]);
                
              }
            }),
          /* subscriptions */component[/* subscriptions */13],
          /* jsElementWrapped */component[/* jsElementWrapped */14]
        ];
}

var text = ViewCommon.text;

var extractString = ViewCommon.extractString;

exports.text = text;
exports.extractString = extractString;
exports.component = component;
exports.Styles = Styles;
exports.make = make;
/* component Not a pure module */
