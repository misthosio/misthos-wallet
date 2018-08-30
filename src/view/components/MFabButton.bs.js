// Generated by BUCKLESCRIPT VERSION 4.0.5, PLEASE EDIT WITH CARE
'use strict';

var Css = require("bs-css/src/Css.js");
var Theme = require("../Theme.bs.js");
var Colors = require("../Colors.bs.js");
var Router = require("../Router.bs.js");
var ViewCommon = require("../ViewCommon.bs.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");
var MaterialUi_Button = require("@jsiebern/bs-material-ui/src/MaterialUi_Button.bs.js");

var component = ReasonReact.statelessComponent("MFabButton");

function button(variant) {
  return Css.style(/* :: */[
              Css.width(Css.px(Theme.space(19))),
              /* :: */[
                Css.height(Css.px(Theme.space(19))),
                /* :: */[
                  Css.borderRadius(Css.px(Theme.space(19))),
                  /* :: */[
                    Css.fontSize(Css.px(16)),
                    /* :: */[
                      Css.unsafe("boxShadow", "none"),
                      /* :: */[
                        Css.unsafe("border", "double 4px transparent"),
                        /* :: */[
                          Css.unsafe("borderImageSlice", "1"),
                          /* :: */[
                            Css.unsafe("backgroundImage", "linear-gradient(white, white), " + (
                                  variant ? Colors.uGradientOrange : Colors.uGradientAqua
                                )),
                            /* :: */[
                              Css.unsafe("backgroundOrigin", "border-box"),
                              /* :: */[
                                Css.unsafe("backgroundClip", "content-box, border-box"),
                                /* [] */0
                              ]
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]);
}

var Styles = /* module */[/* button */button];

function make(variant, route, children) {
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
              var href = Router.Config[/* routeToUrl */1](route);
              return ReasonReact.element(undefined, undefined, MaterialUi_Button.make(button(variant), undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, /* Fab */3502759, undefined, undefined, undefined, undefined, undefined, undefined, (function (param) {
                                return ViewCommon.ignoreEvent((function () {
                                              return ReasonReact.Router[/* push */0](href);
                                            }), param);
                              }), undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, /* array */[children]));
            }),
          /* initialState */component[/* initialState */10],
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */component[/* reducer */12],
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
exports.make = make;
/* component Not a pure module */
