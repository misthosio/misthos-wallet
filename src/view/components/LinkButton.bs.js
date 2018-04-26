// Generated by BUCKLESCRIPT VERSION 2.2.3, PLEASE EDIT WITH CARE
'use strict';

var Router = require("../Router.bs.js");
var MButton = require("./MButton.bs.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");

var component = ReasonReact.statelessComponent("LinkButton");

function make(route, children) {
  var newrecord = component.slice();
  newrecord[/* render */9] = (function () {
      var href = Router.Config[/* routeToUrl */1](route);
      return ReasonReact.element(/* None */0, /* None */0, MButton.make(/* None */0, /* Some */[(function ($$event) {
                          $$event.preventDefault();
                          return ReasonReact.Router[/* push */0](href);
                        })], /* None */0, /* array */[children]));
    });
  return newrecord;
}

exports.component = component;
exports.make = make;
/* component Not a pure module */