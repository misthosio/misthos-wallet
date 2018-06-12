// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE
'use strict';

var ReasonReact = require("reason-react/src/ReasonReact.js");
var Styles = require("material-ui/styles");
var JssProvider = require("react-jss/lib/JssProvider");
var JssInsertionPoint = require("../web/jss-insertion-point");

function make(children) {
  return ReasonReact.wrapJsForReason(JssProvider.default, {
              jss: JssInsertionPoint.default(),
              generateClassName: Styles.createGenerateClassName()
            }, children);
}

exports.make = make;
/* ReasonReact Not a pure module */
