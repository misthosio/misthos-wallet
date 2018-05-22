// Generated by BUCKLESCRIPT VERSION 3.0.0, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var ReRoute = require("reason-reroute/src/ReRoute.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");
var PrimitiveTypes = require("../application/PrimitiveTypes.bs.js");

function routeFromUrl(url) {
  var match = url[/* path */0];
  if (match) {
    switch (match[0]) {
      case "typographystack" : 
          if (match[1]) {
            return /* Home */0;
          } else {
            return /* TypographyStack */2;
          }
      case "ventures" : 
          var match$1 = match[1];
          if (match$1) {
            var id = match$1[0];
            var exit = 0;
            if (id === "new" && !match$1[1]) {
              return /* CreateVenture */1;
            } else {
              exit = 1;
            }
            if (exit === 1) {
              var match$2 = match$1[1];
              if (match$2) {
                switch (match$2[0]) {
                  case "joinvia" : 
                      var match$3 = match$2[1];
                      if (match$3 && !match$3[1]) {
                        return /* JoinVenture */Block.__(1, [
                                  PrimitiveTypes.VentureId[/* fromString */1](id),
                                  PrimitiveTypes.UserId[/* fromString */1](match$3[0])
                                ]);
                      } else {
                        return /* Home */0;
                      }
                  case "partners" : 
                      var match$4 = match$2[1];
                      if (match$4) {
                        if (match$4[1]) {
                          return /* Home */0;
                        } else {
                          return /* Venture */Block.__(0, [
                                    PrimitiveTypes.VentureId[/* fromString */1](id),
                                    /* Partner */Block.__(0, [PrimitiveTypes.ProcessId[/* fromString */1](match$4[0])])
                                  ]);
                        }
                      } else {
                        return /* Venture */Block.__(0, [
                                  PrimitiveTypes.VentureId[/* fromString */1](id),
                                  /* ManagePartners */1
                                ]);
                      }
                  case "payouts" : 
                      var match$5 = match$2[1];
                      if (match$5) {
                        var processId = match$5[0];
                        if (processId === "new") {
                          if (match$5[1]) {
                            return /* Home */0;
                          } else {
                            return /* Venture */Block.__(0, [
                                      PrimitiveTypes.VentureId[/* fromString */1](id),
                                      /* CreatePayout */2
                                    ]);
                          }
                        } else if (match$5[1]) {
                          return /* Home */0;
                        } else {
                          return /* Venture */Block.__(0, [
                                    PrimitiveTypes.VentureId[/* fromString */1](id),
                                    /* Payout */Block.__(1, [PrimitiveTypes.ProcessId[/* fromString */1](processId)])
                                  ]);
                        }
                      } else {
                        return /* Home */0;
                      }
                  case "receive" : 
                      if (match$2[1]) {
                        return /* Home */0;
                      } else {
                        return /* Venture */Block.__(0, [
                                  PrimitiveTypes.VentureId[/* fromString */1](id),
                                  /* Receive */3
                                ]);
                      }
                  default:
                    return /* Home */0;
                }
              } else {
                return /* Venture */Block.__(0, [
                          PrimitiveTypes.VentureId[/* fromString */1](id),
                          /* None */0
                        ]);
              }
            }
            
          } else {
            return /* Home */0;
          }
          break;
      default:
        return /* Home */0;
    }
  } else {
    return /* Home */0;
  }
}

function routeToUrl(route) {
  if (typeof route === "number") {
    switch (route) {
      case 0 : 
          return "/";
      case 1 : 
          return "/ventures/new";
      case 2 : 
          return "/typographystack";
      
    }
  } else if (route.tag) {
    return "/ventures/" + (PrimitiveTypes.VentureId[/* toString */0](route[0]) + ("/joinvia/" + PrimitiveTypes.UserId[/* toString */0](route[1])));
  } else {
    var match = route[1];
    var id = route[0];
    if (typeof match === "number") {
      switch (match) {
        case 0 : 
            return "/ventures/" + PrimitiveTypes.VentureId[/* toString */0](id);
        case 1 : 
            return "/ventures/" + (PrimitiveTypes.VentureId[/* toString */0](id) + "/partners");
        case 2 : 
            return "/ventures/" + (PrimitiveTypes.VentureId[/* toString */0](id) + "/payouts/new");
        case 3 : 
            return "/ventures/" + (PrimitiveTypes.VentureId[/* toString */0](id) + "/receive");
        
      }
    } else if (match.tag) {
      return "/ventures/" + (PrimitiveTypes.VentureId[/* toString */0](id) + ("/payouts/" + PrimitiveTypes.ProcessId[/* toString */0](match[0])));
    } else {
      return "/ventures/" + (PrimitiveTypes.VentureId[/* toString */0](id) + ("/partners/" + PrimitiveTypes.ProcessId[/* toString */0](match[0])));
    }
  }
}

var Config = /* module */[
  /* routeFromUrl */routeFromUrl,
  /* routeToUrl */routeToUrl
];

var include = ReRoute.CreateRouter(Config);

function goTo(route) {
  return ReasonReact.Router[/* push */0](routeToUrl(route));
}

function clickToRoute(route, $$event) {
  $$event.preventDefault();
  return goTo(route);
}

var Container = include[0];

var Link = include[1];

exports.Config = Config;
exports.Container = Container;
exports.Link = Link;
exports.goTo = goTo;
exports.clickToRoute = clickToRoute;
/* include Not a pure module */
