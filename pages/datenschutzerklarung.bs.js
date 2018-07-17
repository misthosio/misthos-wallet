// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE
'use strict';

var Css = require("bs-css/src/Css.js");
var Grid = require("../src/view/components/Grid.bs.js");
var Theme = require("../src/view/Theme.bs.js");
var React = require("react");
var Footer = require("../src/view/Footer.bs.js");
var Header = require("../src/view/Header.bs.js");
var Layout = require("../src/view/Layout.bs.js");
var ScrollList = require("../src/view/components/ScrollList.bs.js");
var ViewCommon = require("../src/view/ViewCommon.bs.js");
var Environment = require("../src/web/Environment.bs.js");
var MTypography = require("../src/view/components/MTypography.bs.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");
var DatenschutzText = require("../src/view/text/DatenschutzText.bs.js");
var WithRoot = require("../src/web/withRoot");

var component = ReasonReact.statelessComponent("datenshutzerklarung");

function line(data) {
  return ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Body1 */-904051921, /* None */0, /* None */0, /* None */0, /* Some */[/* `String */[
                    -976970511,
                    "span"
                  ]], /* None */0, /* array */[ViewCommon.text(data)]));
}

function paragraph(data) {
  return ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Body1 */-904051921, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[ViewCommon.text(data)]));
}

function wrapWithDiv(data) {
  return ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Body1 */-904051921, /* None */0, /* None */0, /* None */0, /* Some */[/* `String */[
                    -976970511,
                    "div"
                  ]], /* None */0, /* array */[data]));
}

function subheading(data) {
  return ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Subheading */148169314, /* None */0, /* Some */[true], /* Some */[true], /* None */0, /* None */0, /* array */[ViewCommon.text(data)]));
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
              return ReasonReact.element(/* None */0, /* None */0, Layout.make(/* Some */[ReasonReact.element(/* None */0, /* None */0, Header.make(/* None */0, /* Some */[environment[/* webDomain */3] + "/"], /* None */0, /* array */[]))], /* None */0, /* None */0, /* Some */[true], /* array */[
                              ReasonReact.element(/* None */0, /* None */0, Grid.make(/* Some */[ViewCommon.text(DatenschutzText.title)], /* None */0, /* None */0, /* None */0, /* Some */[React.createElement("div", {
                                              className: ScrollList.containerStyles + (" " + Css.style(/* :: */[
                                                      Css.paddingBottom(Css.px(Theme.space(4))),
                                                      /* [] */0
                                                    ]))
                                            }, ReasonReact.element(/* None */0, /* None */0, ScrollList.make(/* array */[
                                                      paragraph(DatenschutzText.section1),
                                                      subheading(DatenschutzText.section2Heading),
                                                      React.createElement("div", undefined, line("Justin Carter"), line("Misthos"), line("Dolziger Str. 15"), line("D10247 Berlin"), line("Deutschland"), ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Body1 */-904051921, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[
                                                                    ViewCommon.text("Email: "),
                                                                    React.createElement("a", {
                                                                          href: "mailto:Contact@misthos.io"
                                                                        }, ViewCommon.text("contact@misthos.io"))
                                                                  ])), ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Body1 */-904051921, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* array */[
                                                                    ViewCommon.text("Link zum Impressum: "),
                                                                    React.createElement("a", {
                                                                          href: environment[/* webDomain */3] + "/impressum",
                                                                          target: "_blank"
                                                                        }, ViewCommon.text("https://www.misthos.io/impressum"))
                                                                  ]))),
                                                      subheading(DatenschutzText.section3Heading),
                                                      wrapWithDiv(DatenschutzText.section3),
                                                      subheading(DatenschutzText.section4Heading),
                                                      paragraph(DatenschutzText.section4),
                                                      subheading(DatenschutzText.section5Heading),
                                                      wrapWithDiv(DatenschutzText.section5),
                                                      subheading(DatenschutzText.section6Heading),
                                                      paragraph(DatenschutzText.section6P1),
                                                      paragraph(DatenschutzText.section6P2),
                                                      paragraph(DatenschutzText.section6P3),
                                                      paragraph(DatenschutzText.section6P4),
                                                      paragraph(DatenschutzText.section6P5),
                                                      paragraph(DatenschutzText.section6P6),
                                                      subheading(DatenschutzText.section7Heading),
                                                      paragraph(DatenschutzText.section7),
                                                      subheading(DatenschutzText.section8Heading),
                                                      paragraph(DatenschutzText.section8P1),
                                                      paragraph(DatenschutzText.section8P2),
                                                      subheading(DatenschutzText.section9Heading),
                                                      paragraph(DatenschutzText.section9P1),
                                                      paragraph(DatenschutzText.section9P2),
                                                      subheading(DatenschutzText.section10Heading),
                                                      paragraph(DatenschutzText.section10),
                                                      subheading(DatenschutzText.section11Heading),
                                                      paragraph(DatenschutzText.section11P1),
                                                      paragraph(DatenschutzText.section11P2),
                                                      paragraph(DatenschutzText.section11P3),
                                                      paragraph(DatenschutzText.section11P4),
                                                      paragraph(DatenschutzText.section11P5),
                                                      subheading(DatenschutzText.section12Heading),
                                                      paragraph(DatenschutzText.section12),
                                                      subheading(DatenschutzText.section13Heading),
                                                      paragraph(DatenschutzText.section13),
                                                      subheading(DatenschutzText.section14Heading),
                                                      paragraph(DatenschutzText.section14P1),
                                                      paragraph(DatenschutzText.section14P2),
                                                      paragraph(DatenschutzText.section14P3),
                                                      wrapWithDiv(DatenschutzText.section14P4),
                                                      subheading(DatenschutzText.section15Heading),
                                                      paragraph(DatenschutzText.section15P1),
                                                      paragraph(DatenschutzText.section15P2),
                                                      paragraph(DatenschutzText.section15P3),
                                                      subheading(DatenschutzText.section16Heading),
                                                      paragraph(DatenschutzText.section16P1),
                                                      paragraph(DatenschutzText.section16P2),
                                                      subheading(DatenschutzText.section17Heading),
                                                      paragraph(DatenschutzText.section17P1),
                                                      paragraph(DatenschutzText.section17P2),
                                                      paragraph(DatenschutzText.section17P3),
                                                      subheading(DatenschutzText.section18Heading),
                                                      paragraph(DatenschutzText.section18P1),
                                                      paragraph(DatenschutzText.section18P2),
                                                      paragraph(DatenschutzText.section18P3),
                                                      paragraph(DatenschutzText.section18P4),
                                                      paragraph(DatenschutzText.section18P5),
                                                      paragraph(DatenschutzText.section18P6),
                                                      paragraph(DatenschutzText.section18P7),
                                                      subheading(DatenschutzText.section19Heading),
                                                      wrapWithDiv(DatenschutzText.section19P1),
                                                      paragraph(DatenschutzText.section19P2),
                                                      subheading(DatenschutzText.section20Heading),
                                                      paragraph(DatenschutzText.section20P1),
                                                      paragraph(DatenschutzText.section20P2),
                                                      subheading(DatenschutzText.section21Heading),
                                                      paragraph(DatenschutzText.section21P1),
                                                      paragraph(DatenschutzText.section21P2),
                                                      subheading(DatenschutzText.section22Heading),
                                                      wrapWithDiv(DatenschutzText.section22),
                                                      subheading(DatenschutzText.section23Heading),
                                                      wrapWithDiv(DatenschutzText.section23),
                                                      subheading(DatenschutzText.section24Heading),
                                                      wrapWithDiv(DatenschutzText.section24),
                                                      wrapWithDiv(DatenschutzText.section25)
                                                    ])))], /* None */0, /* None */0, /* None */0, /* array */[])),
                              ReasonReact.element(/* None */0, /* None */0, Footer.make(/* array */[]))
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

var T = 0;

exports.text = text;
exports.extractString = extractString;
exports.component = component;
exports.line = line;
exports.paragraph = paragraph;
exports.wrapWithDiv = wrapWithDiv;
exports.subheading = subheading;
exports.T = T;
exports.environment = environment;
exports.make = make;
exports.$$default = $$default;
exports.default = $$default;
exports.__esModule = true;
/* component Not a pure module */
