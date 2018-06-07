// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE
'use strict';

var Css = require("bs-css/src/Css.js");
var React = require("react");
var MButton = require("./components/MButton.bs.js");
var ViewCommon = require("./ViewCommon.bs.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");
var MaterialUi_Grid = require("@jsiebern/bs-material-ui/src/MaterialUi_Grid.bs.js");
var MaterialUi_Typography = require("@jsiebern/bs-material-ui/src/MaterialUi_Typography.bs.js");

var component = ReasonReact.statelessComponent("PublicHome");

var fullHeight = Css.style(/* :: */[
      Css.height(/* `percent */[
            -119887163,
            100.0
          ]),
      /* [] */0
    ]);

var display4 = Css.style(/* :: */[
      Css.paddingBottom(/* `vw */[
            26433,
            1.5
          ]),
      /* [] */0
    ]);

var background = Css.style(/* :: */[
      Css.backgroundRepeat(Css.noRepeat),
      /* :: */[
        Css.backgroundSize(/* `size */[
              -866934591,
              /* tuple */[
                /* `px */[
                  25096,
                  584
                ],
                /* `px */[
                  25096,
                  419
                ]
              ]
            ]),
        /* [] */0
      ]
    ]);

var Styles = /* module */[
  /* fullHeight */fullHeight,
  /* display4 */display4,
  /* background */background
];

function make(onSignIn, _) {
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
              return ReasonReact.element(/* None */0, /* None */0, MaterialUi_Grid.make(/* None */0, /* Some */[/* Center */980392437], /* Some */[background], /* None */0, /* Some */[true], /* Some */[/* Row */4102650], /* None */0, /* None */0, /* Some */[/* Center */980392437], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[ReasonReact.element(/* None */0, /* None */0, MaterialUi_Grid.make(/* None */0, /* None */0, /* None */0, /* None */0, /* Some */[true], /* Some */[/* Row */4102650], /* None */0, /* None */0, /* Some */[/* Center */980392437], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[
                                        ReasonReact.element(/* None */0, /* None */0, MaterialUi_Grid.make(/* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* Some */[true], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* Some */[/* V10 */9], /* None */0, /* None */0, /* None */0, /* array */[
                                                  ReasonReact.element(/* None */0, /* None */0, MaterialUi_Typography.make(/* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* Some */[/* Display4 */-11760686], /* None */0, /* None */0, /* array */[ViewCommon.text("Distribute Funds")])),
                                                  ReasonReact.element(/* None */0, /* None */0, MaterialUi_Typography.make(/* None */0, /* Some */[display4], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* Some */[/* Display4 */-11760686], /* None */0, /* None */0, /* array */[ViewCommon.text("with misthos.")]))
                                                ])),
                                        ReasonReact.element(/* None */0, /* None */0, MaterialUi_Grid.make(/* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* Some */[true], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* Some */[/* V10 */9], /* None */0, /* None */0, /* None */0, /* array */[ReasonReact.element(/* None */0, /* None */0, MaterialUi_Grid.make(/* None */0, /* None */0, /* None */0, /* None */0, /* Some */[true], /* None */0, /* None */0, /* None */0, /* Some */[/* Space_Between */959915471], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[
                                                            ReasonReact.element(/* None */0, /* None */0, MaterialUi_Grid.make(/* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* Some */[true], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* Some */[/* V8 */7], /* None */0, /* None */0, /* None */0, /* array */[
                                                                      ReasonReact.element(/* None */0, /* None */0, MaterialUi_Typography.make(/* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* Some */[/* Display1 */-11760689], /* None */0, /* None */0, /* array */[ViewCommon.text("Misthos is the only multi-sig Bitcoin wallet that lets you change co-singers in a fast and friction-less way.")])),
                                                                      React.createElement("br", undefined),
                                                                      ReasonReact.element(/* None */0, /* None */0, MaterialUi_Typography.make(/* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* Some */[/* Display1 */-11760689], /* None */0, /* None */0, /* array */[ViewCommon.text("Use it for projects. Use it for payments.")]))
                                                                    ])),
                                                            ReasonReact.element(/* None */0, /* None */0, MaterialUi_Grid.make(/* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* Some */[true], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* Some */[/* V3 */2], /* None */0, /* None */0, /* None */0, /* array */[ReasonReact.element(/* None */0, /* None */0, MaterialUi_Grid.make(/* None */0, /* Some */[/* Flex_End */-403022699], /* Some */[fullHeight], /* None */0, /* Some */[true], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[ReasonReact.element(/* None */0, /* None */0, MButton.make(/* Some */[/* Inherit */-72987685], /* Some */[onSignIn], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */["Sign In with Blockstack"]))]))]))
                                                          ]))]))
                                      ]))]));
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

exports.text = text;
exports.extractString = extractString;
exports.component = component;
exports.Styles = Styles;
exports.make = make;
/* component Not a pure module */
