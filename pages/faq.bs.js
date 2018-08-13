// Generated by BUCKLESCRIPT VERSION 4.0.5, PLEASE EDIT WITH CARE
'use strict';

var Css = require("bs-css/src/Css.js");
var Grid = require("../src/view/components/Grid.bs.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Icons = require("../src/view/Icons.bs.js");
var Theme = require("../src/view/Theme.bs.js");
var React = require("react");
var Footer = require("../src/view/Footer.bs.js");
var Header = require("../src/view/Header.bs.js");
var Layout = require("../src/view/Layout.bs.js");
var FaqText = require("../src/view/text/FaqText.bs.js");
var ScrollList = require("../src/view/components/ScrollList.bs.js");
var ViewCommon = require("../src/view/ViewCommon.bs.js");
var Environment = require("../src/web/Environment.bs.js");
var MTypography = require("../src/view/components/MTypography.bs.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");
var Js_primitive = require("bs-platform/lib/js/js_primitive.js");
var WithRoot = require("../src/web/withRoot");
var MaterialUi_ExpansionPanel = require("@jsiebern/bs-material-ui/src/MaterialUi_ExpansionPanel.bs.js");
var MaterialUi_ExpansionPanelDetails = require("@jsiebern/bs-material-ui/src/MaterialUi_ExpansionPanelDetails.bs.js");
var MaterialUi_ExpansionPanelSummary = require("@jsiebern/bs-material-ui/src/MaterialUi_ExpansionPanelSummary.bs.js");

var component = ReasonReact.statelessComponent("faq");

function line(data) {
  return ReasonReact.element(undefined, undefined, MTypography.make(/* Body1 */-904051921, undefined, undefined, undefined, undefined, undefined, /* array */[ViewCommon.text(data)]));
}

function paragraph(data) {
  return ReasonReact.element(undefined, undefined, MTypography.make(/* Body1 */-904051921, undefined, true, undefined, undefined, undefined, /* array */[ViewCommon.text(data)]));
}

function wrapWithDiv(data) {
  return ReasonReact.element(undefined, undefined, MTypography.make(/* Body1 */-904051921, undefined, undefined, undefined, /* `String */[
                  -976970511,
                  "div"
                ], undefined, /* array */[data]));
}

var environment = Environment.get(/* () */0);

function make() {
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
              return ReasonReact.element(undefined, undefined, Layout.make(Js_primitive.some(ReasonReact.element(undefined, undefined, Header.make(undefined, environment[/* webDomain */3] + "/", undefined, /* array */[]))), undefined, undefined, true, /* array */[
                              ReasonReact.element(undefined, undefined, Grid.make(Js_primitive.some(ViewCommon.text("frequently asked questions")), undefined, undefined, undefined, Js_primitive.some(React.createElement("div", {
                                                className: ScrollList.containerStyles + (" " + Css.style(/* :: */[
                                                        Css.paddingBottom(Css.px(Theme.space(4))),
                                                        /* [] */0
                                                      ]))
                                              }, ReasonReact.element(undefined, undefined, ScrollList.make(/* array */[$$Array.map((function (item) {
                                                                return ReasonReact.element(undefined, undefined, MaterialUi_ExpansionPanel.make(undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, /* array */[
                                                                                ReasonReact.element(undefined, undefined, MaterialUi_ExpansionPanelSummary.make(undefined, undefined, undefined, Js_primitive.some(Icons.chevronDown), undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, /* array */[ReasonReact.element(undefined, undefined, MTypography.make(/* Subheading */148169314, undefined, undefined, undefined, undefined, undefined, /* array */[ViewCommon.text(item[/* q */0])]))])),
                                                                                ReasonReact.element(undefined, undefined, MaterialUi_ExpansionPanelDetails.make(Css.style(/* :: */[
                                                                                              Css.flexDirection(Css.column),
                                                                                              /* [] */0
                                                                                            ]), undefined, undefined, /* array */[$$Array.map((function (line) {
                                                                                                  if (line.tag) {
                                                                                                    return wrapWithDiv(line[0]);
                                                                                                  } else {
                                                                                                    return paragraph(line[0]);
                                                                                                  }
                                                                                                }), item[/* a */1])]))
                                                                              ]));
                                                              }), FaqText.faq)])))), undefined, undefined, undefined, /* array */[])),
                              ReasonReact.element(undefined, undefined, Footer.make(/* array */[]))
                            ]));
            }),
          /* initialState */component[/* initialState */10],
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */component[/* reducer */12],
          /* subscriptions */component[/* subscriptions */13],
          /* jsElementWrapped */component[/* jsElementWrapped */14]
        ];
}

var $$default = WithRoot.default(ReasonReact.wrapReasonForJs(component, (function () {
            return make(/* array */[]);
          })));

var text = ViewCommon.text;

var extractString = ViewCommon.extractString;

var ignoreEvent = ViewCommon.ignoreEvent;

var Details = 0;

var T = 0;

exports.text = text;
exports.extractString = extractString;
exports.ignoreEvent = ignoreEvent;
exports.component = component;
exports.line = line;
exports.paragraph = paragraph;
exports.wrapWithDiv = wrapWithDiv;
exports.Details = Details;
exports.environment = environment;
exports.T = T;
exports.make = make;
exports.$$default = $$default;
exports.default = $$default;
exports.__esModule = true;
/* component Not a pure module */
