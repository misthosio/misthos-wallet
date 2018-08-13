// Generated by BUCKLESCRIPT VERSION 4.0.5, PLEASE EDIT WITH CARE
'use strict';

var React = require("react");
var Footer = require("../src/view/Footer.bs.js");
var Layout = require("../src/view/Layout.bs.js");
var Session = require("../src/web/Session.bs.js");
var MisthosIs = require("../src/view/components/MisthosIs.bs.js");
var PublicHome = require("../src/view/PublicHome.bs.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");
var WithRoot = require("../src/web/withRoot");

var component = ReasonReact.statelessComponent("index");

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
              return ReasonReact.element(undefined, undefined, Layout.make(undefined, undefined, undefined, true, /* array */[
                              ReasonReact.element(undefined, undefined, PublicHome.make((function () {
                                          Session.signIn(/* () */0);
                                          return /* () */0;
                                        }), /* array */[])),
                              ReasonReact.element(undefined, undefined, MisthosIs.make("Collaborative", "Join a Venture or start your own. Propose and endorse decisions based on team consensus.", React.createElement("img", {
                                            height: "480px",
                                            src: "/static/img/misthos-gif-01-v02.gif",
                                            width: "350px"
                                          }), undefined, /* array */[])),
                              ReasonReact.element(undefined, undefined, MisthosIs.make("Intuitive", "Focus on simplicity. Complete tasks quickly with an easy-to-use interface.", React.createElement("img", {
                                            height: "480px",
                                            src: "/static/img/misthos-gif-02-v01.gif",
                                            width: "350px"
                                          }), undefined, /* array */[])),
                              ReasonReact.element(undefined, undefined, MisthosIs.make("Dynamic", "See all Venture activity instantaneously. Manage Partners and create payouts with ease.", React.createElement("img", {
                                            height: "480px",
                                            src: "/static/img/misthos-gif-03-v01.gif",
                                            width: "350px"
                                          }), true, /* array */[])),
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

exports.component = component;
exports.make = make;
exports.$$default = $$default;
exports.default = $$default;
exports.__esModule = true;
/* component Not a pure module */
