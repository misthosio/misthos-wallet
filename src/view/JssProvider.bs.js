// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var ReasonReact = require("reason-react/src/ReasonReact.js");
var Core = require("@material-ui/core");
var JssProvider = require("react-jss/lib/JssProvider");
var JssInsertionPoint = require("../web/jss-insertion-point");

function jss(prim) {
  return JssInsertionPoint.default();
}

function make(children) {
  return ReasonReact.wrapJsForReason(JssProvider.default, {
              jss: JssInsertionPoint.default(),
              generateClassName: Core.createGenerateClassName()
            }, children);
}

exports.jss = jss;
exports.make = make;
/* ReasonReact Not a pure module */
