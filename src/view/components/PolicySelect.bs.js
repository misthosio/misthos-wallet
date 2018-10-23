// Generated by BUCKLESCRIPT VERSION 4.0.6, PLEASE EDIT WITH CARE
'use strict';

var Css = require("bs-css/src/Css.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Theme = require("../Theme.bs.js");
var Policy = require("../../application/Policy.bs.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var ViewCommon = require("../ViewCommon.bs.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");
var MaterialUi_Grid = require("@jsiebern/bs-material-ui/src/MaterialUi_Grid.bs.js");
var MaterialUi_Input = require("@jsiebern/bs-material-ui/src/MaterialUi_Input.bs.js");
var MaterialUi_Select = require("@jsiebern/bs-material-ui/src/MaterialUi_Select.bs.js");
var MaterialUi_MenuItem = require("@jsiebern/bs-material-ui/src/MaterialUi_MenuItem.bs.js");
var MaterialUi_InputLabel = require("@jsiebern/bs-material-ui/src/MaterialUi_InputLabel.bs.js");
var MaterialUi_FormControl = require("@jsiebern/bs-material-ui/src/MaterialUi_FormControl.bs.js");

var component = ReasonReact.reducerComponent("PolicySelect");

var container = Css.style(/* :: */[
      Css.marginBottom(Css.px(Theme.space(2))),
      /* [] */0
    ]);

var flex = Css.style(/* :: */[
      Css.flex(1),
      /* [] */0
    ]);

var Styles = /* module */[
  /* container */container,
  /* flex */flex
];

function policyTypeToString(param) {
  if (typeof param === "number") {
    if (param === 0) {
      return "Unanimous";
    } else {
      return "Unanimous minus 1";
    }
  } else if (param.tag) {
    return "At least";
  } else {
    return "Percentage";
  }
}

function stringToPolicy(param) {
  switch (param) {
    case "At least" : 
        return Policy.atLeast(1);
    case "Percentage" : 
        return Policy.percentage(51);
    case "Unanimous" : 
        return Policy.unanimous;
    case "Unanimous minus 1" : 
        return Policy.unanimousMinusOne;
    default:
      return Policy.unanimous;
  }
}

var policyOptions = /* array */[
  Policy.atLeast(1),
  Policy.percentage(51),
  Policy.unanimousMinusOne,
  Policy.unanimous
];

function updatePolicyWithN(n, policy) {
  if (typeof policy === "number") {
    return /* tuple */[
            policy,
            ""
          ];
  } else if (policy.tag) {
    var match = n < 0;
    var n$1 = match ? 1 : n;
    return /* tuple */[
            Policy.atLeast(n$1),
            String(n$1)
          ];
  } else {
    var match$1 = n < 0;
    var n$2;
    if (match$1) {
      n$2 = 0;
    } else {
      var match$2 = n > 100;
      n$2 = match$2 ? 100 : n;
    }
    return /* tuple */[
            Policy.percentage(n$2),
            String(n$2)
          ];
  }
}

function extractPolicyNumber(param) {
  if (typeof param === "number") {
    return "";
  } else {
    return String(param[0][/* n */0]);
  }
}

function make(label, initialValue, onChange, _) {
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
              var state = param[/* state */1];
              var policyMenuItems = Belt_Array.mapU(policyOptions, (function (p) {
                      return ReasonReact.element(undefined, undefined, MaterialUi_MenuItem.make(undefined, undefined, undefined, undefined, /* `String */[
                                      -976970511,
                                      policyTypeToString(p)
                                    ], undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, /* array */[ViewCommon.text(policyTypeToString(p))]));
                    }));
              var match = state[/* selectedPolicy */0];
              var tmp;
              tmp = typeof match === "number" ? null : (
                  match.tag ? ReasonReact.element(undefined, undefined, MaterialUi_FormControl.make(undefined, undefined, undefined, state[/* inputNumber */1] === "", undefined, undefined, undefined, undefined, undefined, undefined, /* array */[
                              ReasonReact.element(undefined, undefined, MaterialUi_InputLabel.make(undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, /* array */["N ="])),
                              ReasonReact.element(undefined, undefined, MaterialUi_Input.make(undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, (function (e) {
                                          return Curry._1(send, /* SelectPolicyNumber */Block.__(1, [ViewCommon.extractString(e)]));
                                        }), undefined, undefined, undefined, undefined, undefined, undefined, undefined, /* `String */[
                                        -976970511,
                                        state[/* inputNumber */1]
                                      ], undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, /* array */[]))
                            ])) : ReasonReact.element(undefined, undefined, MaterialUi_FormControl.make(undefined, undefined, undefined, state[/* inputNumber */1] === "", undefined, undefined, undefined, undefined, undefined, undefined, /* array */[
                              ReasonReact.element(undefined, undefined, MaterialUi_InputLabel.make(undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, /* array */["% ="])),
                              ReasonReact.element(undefined, undefined, MaterialUi_Input.make(undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, (function (e) {
                                          return Curry._1(send, /* SelectPolicyNumber */Block.__(1, [ViewCommon.extractString(e)]));
                                        }), undefined, undefined, undefined, undefined, undefined, undefined, undefined, /* `String */[
                                        -976970511,
                                        state[/* inputNumber */1]
                                      ], undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, /* array */[]))
                            ]))
                );
              return ReasonReact.element(undefined, undefined, MaterialUi_Grid.make(undefined, undefined, container, undefined, true, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, /* array */[
                              ReasonReact.element(undefined, undefined, MaterialUi_FormControl.make(flex, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, /* array */[
                                        ReasonReact.element(undefined, undefined, MaterialUi_InputLabel.make(undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, /* array */[ViewCommon.text(label)])),
                                        ReasonReact.element(undefined, undefined, MaterialUi_Select.make(undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, (function (e, _) {
                                                    return Curry._1(send, /* SelectPolicyType */Block.__(0, [stringToPolicy(ViewCommon.extractString(e))]));
                                                  }), undefined, undefined, undefined, undefined, undefined, /* `String */[
                                                  -976970511,
                                                  policyTypeToString(state[/* selectedPolicy */0])
                                                ], undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, /* array */[policyMenuItems]))
                                      ])),
                              tmp
                            ]));
            }),
          /* initialState */(function () {
              return /* record */[
                      /* selectedPolicy */initialValue,
                      /* inputNumber */extractPolicyNumber(initialValue)
                    ];
            }),
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */(function (action, state) {
              if (action.tag) {
                var n = action[0];
                if (n === "") {
                  Curry._1(onChange, /* InvalidSelection */0);
                  return /* Update */Block.__(0, [/* record */[
                              /* selectedPolicy */state[/* selectedPolicy */0],
                              /* inputNumber */""
                            ]]);
                } else {
                  try {
                    var match = updatePolicyWithN(Caml_format.caml_int_of_string(n), state[/* selectedPolicy */0]);
                    var selectedPolicy = match[0];
                    var state_001 = /* inputNumber */match[1];
                    var state$1 = /* record */[
                      /* selectedPolicy */selectedPolicy,
                      state_001
                    ];
                    Curry._1(onChange, /* ValidSelection */[selectedPolicy]);
                    return /* Update */Block.__(0, [state$1]);
                  }
                  catch (exn){
                    return /* NoUpdate */0;
                  }
                }
              } else {
                var policy = action[0];
                var state_001$1 = /* inputNumber */extractPolicyNumber(policy);
                var state$2 = /* record */[
                  /* selectedPolicy */policy,
                  state_001$1
                ];
                Curry._1(onChange, /* ValidSelection */[policy]);
                return /* Update */Block.__(0, [state$2]);
              }
            }),
          /* jsElementWrapped */component[/* jsElementWrapped */13]
        ];
}

var text = ViewCommon.text;

var extractString = ViewCommon.extractString;

var ignoreEvent = ViewCommon.ignoreEvent;

exports.text = text;
exports.extractString = extractString;
exports.ignoreEvent = ignoreEvent;
exports.component = component;
exports.Styles = Styles;
exports.policyTypeToString = policyTypeToString;
exports.stringToPolicy = stringToPolicy;
exports.policyOptions = policyOptions;
exports.updatePolicyWithN = updatePolicyWithN;
exports.extractPolicyNumber = extractPolicyNumber;
exports.make = make;
/* component Not a pure module */
