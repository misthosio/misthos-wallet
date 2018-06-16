// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE
'use strict';

var Css = require("bs-css/src/Css.js");
var Grid = require("../src/view/components/Grid.bs.js");
var Text = require("../src/Text.bs.js");
var Theme = require("../src/view/Theme.bs.js");
var React = require("react");
var Footer = require("../src/view/Footer.bs.js");
var Layout = require("../src/view/Layout.bs.js");
var ScrollList = require("../src/view/components/ScrollList.bs.js");
var ViewCommon = require("../src/view/ViewCommon.bs.js");
var Environment = require("../src/web/Environment.bs.js");
var MTypography = require("../src/view/components/MTypography.bs.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");
var WithRoot = require("../src/web/withRoot");

var component = ReasonReact.statelessComponent("datenshutzerklarung");

function line(data) {
  return ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Body1 */-904051921, /* None */0, /* None */0, /* None */0, /* array */[ViewCommon.text(data)]));
}

function paragraph(data) {
  return ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Body1 */-904051921, /* None */0, /* None */0, /* None */0, /* array */[React.createElement("p", undefined, ViewCommon.text(data))]));
}

function subheading(data) {
  return ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Subheading */148169314, /* None */0, /* None */0, /* None */0, /* array */[ViewCommon.text(data)]));
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
              return ReasonReact.element(/* None */0, /* None */0, Layout.make(/* None */0, /* None */0, /* Some */[true], /* array */[
                              ReasonReact.element(/* None */0, /* None */0, Grid.make(/* Some */[ViewCommon.text(Text.DatenschutzErklaerung[/* title */0])], /* None */0, /* None */0, /* None */0, /* Some */[React.createElement("div", {
                                              className: ScrollList.containerStyles + (" " + Css.style(/* :: */[
                                                      Css.paddingBottom(Css.px(Theme.space(4))),
                                                      /* [] */0
                                                    ]))
                                            }, ReasonReact.element(/* None */0, /* None */0, ScrollList.make(/* array */[
                                                      paragraph(Text.DatenschutzErklaerung[/* section1 */1]),
                                                      subheading(Text.DatenschutzErklaerung[/* section2Heading */2]),
                                                      React.createElement("p", undefined, line("Justin Carter"), line("Misthos"), line("Dolziger Str. 15"), line("D10247 Berlin"), line("Deutschland"), ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Body1 */-904051921, /* None */0, /* None */0, /* None */0, /* array */[
                                                                    ViewCommon.text("Email: "),
                                                                    React.createElement("a", {
                                                                          href: "mailto:Contact@misthos.io"
                                                                        }, ViewCommon.text("contact@misthos.io"))
                                                                  ])), ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Body1 */-904051921, /* None */0, /* None */0, /* None */0, /* array */[
                                                                    ViewCommon.text("Link zum Impressum: "),
                                                                    React.createElement("a", {
                                                                          href: environment[/* webDomain */3] + "/impressum",
                                                                          target: "_blank"
                                                                        }, ViewCommon.text("https://www.misthos.io/impressum"))
                                                                  ]))),
                                                      subheading(Text.DatenschutzErklaerung[/* section3Heading */3]),
                                                      ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Body1 */-904051921, /* None */0, /* None */0, /* None */0, /* array */[Text.DatenschutzErklaerung[/* section3 */4]])),
                                                      subheading(Text.DatenschutzErklaerung[/* section4Heading */5]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section4 */6]),
                                                      subheading(Text.DatenschutzErklaerung[/* section5Heading */7]),
                                                      ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Body1 */-904051921, /* None */0, /* None */0, /* None */0, /* array */[Text.DatenschutzErklaerung[/* section5 */8]])),
                                                      subheading(Text.DatenschutzErklaerung[/* section6Heading */9]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section6P1 */10]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section6P2 */11]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section6P3 */12]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section6P4 */13]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section6P5 */14]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section6P6 */15]),
                                                      subheading(Text.DatenschutzErklaerung[/* section7Heading */16]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section7 */17]),
                                                      subheading(Text.DatenschutzErklaerung[/* section8Heading */18]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section8P1 */19]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section8P2 */20]),
                                                      subheading(Text.DatenschutzErklaerung[/* section9Heading */21]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section9P1 */22]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section9P2 */23]),
                                                      subheading(Text.DatenschutzErklaerung[/* section10Heading */24]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section10 */25]),
                                                      subheading(Text.DatenschutzErklaerung[/* section11Heading */26]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section11P1 */27]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section11P2 */28]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section11P3 */29]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section11P4 */30]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section11P5 */31]),
                                                      subheading(Text.DatenschutzErklaerung[/* section12Heading */32]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section12 */33]),
                                                      subheading(Text.DatenschutzErklaerung[/* section13Heading */34]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section13 */35]),
                                                      subheading(Text.DatenschutzErklaerung[/* section14Heading */36]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section14P1 */37]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section14P2 */38]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section14P3 */39]),
                                                      ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Body1 */-904051921, /* None */0, /* None */0, /* None */0, /* array */[Text.DatenschutzErklaerung[/* section14P4 */40]])),
                                                      subheading(Text.DatenschutzErklaerung[/* section15Heading */41]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section15P1 */42]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section15P2 */43]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section15P3 */44]),
                                                      subheading(Text.DatenschutzErklaerung[/* section16Heading */46]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section16P1 */47]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section16P2 */48]),
                                                      subheading(Text.DatenschutzErklaerung[/* section17Heading */49]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section17P1 */50]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section17P2 */51]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section17P3 */52]),
                                                      subheading(Text.DatenschutzErklaerung[/* section18Heading */53]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section18P1 */54]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section18P2 */55]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section18P3 */56]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section18P4 */57]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section18P5 */58]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section18P6 */59]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section18P7 */60]),
                                                      subheading(Text.DatenschutzErklaerung[/* section19Heading */61]),
                                                      ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Body1 */-904051921, /* None */0, /* None */0, /* None */0, /* array */[Text.DatenschutzErklaerung[/* section19P1 */62]])),
                                                      paragraph(Text.DatenschutzErklaerung[/* section19P2 */63]),
                                                      subheading(Text.DatenschutzErklaerung[/* section20Heading */64]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section20P1 */65]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section20P2 */66]),
                                                      subheading(Text.DatenschutzErklaerung[/* section21Heading */67]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section21P1 */68]),
                                                      paragraph(Text.DatenschutzErklaerung[/* section21P2 */69]),
                                                      subheading(Text.DatenschutzErklaerung[/* section22Heading */70]),
                                                      ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Body1 */-904051921, /* None */0, /* None */0, /* None */0, /* array */[Text.DatenschutzErklaerung[/* section22 */71]])),
                                                      subheading(Text.DatenschutzErklaerung[/* section23Heading */72]),
                                                      ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Body1 */-904051921, /* None */0, /* None */0, /* None */0, /* array */[Text.DatenschutzErklaerung[/* section23 */73]])),
                                                      subheading(Text.DatenschutzErklaerung[/* section24Heading */74]),
                                                      ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Body1 */-904051921, /* None */0, /* None */0, /* None */0, /* array */[Text.DatenschutzErklaerung[/* section24 */75]])),
                                                      ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Body1 */-904051921, /* None */0, /* None */0, /* None */0, /* array */[Text.DatenschutzErklaerung[/* section25 */76]]))
                                                    ])))], /* None */0, /* array */[])),
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
exports.subheading = subheading;
exports.T = T;
exports.environment = environment;
exports.make = make;
exports.$$default = $$default;
exports.default = $$default;
exports.__esModule = true;
/* component Not a pure module */