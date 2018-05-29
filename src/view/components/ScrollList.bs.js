// Generated by BUCKLESCRIPT VERSION 3.1.4, PLEASE EDIT WITH CARE
'use strict';

var Css = require("bs-css/src/Css.js");
var React = require("react");
var ViewCommon = require("../ViewCommon.bs.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");
var ReactCustomScrollbars = require("react-custom-scrollbars");

var foo = Css.style(/* [] */0);

var Styles = /* module */[/* foo */foo];

function renderView() {
  return React.createElement("div", {
              className: foo
            });
}

function make(children) {
  return ReasonReact.wrapJsForReason(ReactCustomScrollbars.default, {
              autoHeight: true,
              autoHide: true
            }, children);
}

var text = ViewCommon.text;

var extractString = ViewCommon.extractString;

exports.text = text;
exports.extractString = extractString;
exports.Styles = Styles;
exports.renderView = renderView;
exports.make = make;
/* foo Not a pure module */