// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var Theme = require("./Theme.bs.js");
var JssProvider = require("./JssProvider.bs.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");
var MaterialUi_CssBaseline = require("@jsiebern/bs-material-ui/src/MaterialUi_CssBaseline.bs.js");
var MaterialUi_MuiThemeProvider = require("@jsiebern/bs-material-ui/src/MaterialUi_MuiThemeProvider.bs.js");

var component = ReasonReact.statelessComponent("PageLayout");

var theme = Theme.toJsUnsafe(Theme.theme(undefined, /* () */0));

function make(children) {
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
          /* render */(function (_self) {
              return ReasonReact.element(undefined, undefined, JssProvider.make(/* array */[ReasonReact.element(undefined, undefined, MaterialUi_MuiThemeProvider.make(undefined, undefined, undefined, /* `ObjectGeneric */[
                                        -317959944,
                                        theme
                                      ], /* array */[ReasonReact.element(undefined, undefined, MaterialUi_CssBaseline.make(/* array */[children]))]))]));
            }),
          /* initialState */component[/* initialState */10],
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */component[/* reducer */12],
          /* jsElementWrapped */component[/* jsElementWrapped */13]
        ];
}

exports.component = component;
exports.theme = theme;
exports.make = make;
/* component Not a pure module */
