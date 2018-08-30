// Generated by BUCKLESCRIPT VERSION 4.0.5, PLEASE EDIT WITH CARE
'use strict';

var Css = require("bs-css/src/Css.js");
var Theme = require("../Theme.bs.js");
var React = require("react");
var Colors = require("../Colors.bs.js");
var ViewCommon = require("../ViewCommon.bs.js");
var MTypography = require("./MTypography.bs.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");

var component = ReasonReact.statelessComponent("VentureInfoBox");

var infoBox = Css.style(/* :: */[
      Css.border(Css.px(2), Css.solid, Colors.black),
      /* :: */[
        Css.padding4(Css.px(0), Css.px(Theme.space(4)), Css.px(Theme.space(4)), Css.px(Theme.space(4))),
        /* [] */0
      ]
    ]);

var Styles = /* module */[/* infoBox */infoBox];

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
              return React.createElement("div", {
                          className: infoBox
                        }, ReasonReact.element(undefined, undefined, MTypography.make(/* Title */594052472, undefined, true, true, undefined, undefined, /* array */[ViewCommon.text("What can you do with a venture?")])), ReasonReact.element(undefined, undefined, MTypography.make(/* Body2 */-904051920, undefined, true, undefined, undefined, undefined, /* array */[ViewCommon.text("\n                 • Your Venture contains a multisig bitcoin wallet that you can share access to by adding and removing Partners\n                ")])), ReasonReact.element(undefined, undefined, MTypography.make(/* Body2 */-904051920, undefined, true, undefined, undefined, undefined, /* array */[ViewCommon.text("\n                 • As a team, you and your Partners can receive income and distribute payouts\n                ")])), ReasonReact.element(undefined, undefined, MTypography.make(/* Body2 */-904051920, undefined, true, undefined, undefined, undefined, /* array */[ViewCommon.text("\n                 •  All decisions withn a Venture are executed when a sufficient team consensus has been achieved\n                ")])));
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
