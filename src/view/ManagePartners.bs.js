// Generated by BUCKLESCRIPT VERSION 3.0.0, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Block = require("bs-platform/lib/js/block.js");
var Body2 = require("./components/Body2.bs.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Utils = require("../utils/Utils.bs.js");
var React = require("react");
var MInput = require("./components/MInput.bs.js");
var $$String = require("bs-platform/lib/js/string.js");
var MButton = require("./components/MButton.bs.js");
var Partner = require("./components/Partner.bs.js");
var MaterialUi = require("@jsiebern/bs-material-ui/src/MaterialUi.bs.js");
var MTypography = require("./components/MTypography.bs.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");
var PrimitiveTypes = require("../application/PrimitiveTypes.bs.js");

function changeNewPartnerId($$event) {
  return /* ChangeNewPartnerId */Block.__(0, [$$event.target.value]);
}

var component = ReasonReact.reducerComponent("ManagePartners");

function make(joinVentureUrl, viewData, commands, session, _) {
  return /* record */[
          /* debugName */component[/* debugName */0],
          /* reactClassInternal */component[/* reactClassInternal */1],
          /* handedOffState */component[/* handedOffState */2],
          /* willReceiveProps */(function (param) {
              return /* record */[
                      /* viewData */viewData,
                      /* prospectId */param[/* state */1][/* prospectId */1],
                      /* currentUser */session[/* userId */0]
                    ];
            }),
          /* didMount */component[/* didMount */4],
          /* didUpdate */component[/* didUpdate */5],
          /* willUnmount */component[/* willUnmount */6],
          /* willUpdate */component[/* willUpdate */7],
          /* shouldUpdate */component[/* shouldUpdate */8],
          /* render */(function (param) {
              var send = param[/* send */3];
              var state = param[/* state */1];
              var partners = $$Array.of_list(List.map((function (partner) {
                          return ReasonReact.element(/* Some */[PrimitiveTypes.UserId[/* toString */0](partner[/* userId */0])], /* None */0, Partner.make(partner, /* array */[]));
                        }), state[/* viewData */0][/* partners */0]));
              var partnersOld = $$Array.of_list(List.map((function (m) {
                          var match = PrimitiveTypes.UserId[/* eq */5](state[/* currentUser */2], m[/* userId */0]);
                          var match$1 = List.exists((function (p) {
                                  return PrimitiveTypes.UserId[/* eq */5](p[/* userId */1], m[/* userId */0]);
                                }), state[/* viewData */0][/* removalProspects */2]);
                          return React.createElement("li", {
                                      key: PrimitiveTypes.UserId[/* toString */0](m[/* userId */0])
                                    }, React.createElement("div", undefined, Utils.text(PrimitiveTypes.UserId[/* toString */0](m[/* userId */0])), match || match$1 ? null : React.createElement("button", {
                                                onClick: (function () {
                                                    return Curry._1(send, /* RemovePartner */Block.__(1, [m[/* userId */0]]));
                                                  })
                                              }, Utils.text("Propose Removal"))));
                        }), state[/* viewData */0][/* partners */0]));
              return ReasonReact.element(/* None */0, /* None */0, Body2.make(/* Some */[/* :: */[
                                "Add a partner",
                                /* :: */[
                                  "Remove a partner",
                                  /* [] */0
                                ]
                              ]], React.createElement("div", undefined, ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Body2 */-904051920, /* None */0, /* array */[Utils.text("\n                 To propose adding a new partner to the venture,\n                 enter a valid Blockstack ID below. When enough partners endorse this proposal,\n                 the partner will be added.\n                ")])), ReasonReact.element(/* None */0, /* None */0, MInput.make(/* Some */["Enter a Blockstack ID"], /* Some */[/* `String */[
                                            -976970511,
                                            state[/* prospectId */1]
                                          ]], /* Some */[(function (e) {
                                              return Curry._1(send, /* ChangeNewPartnerId */Block.__(0, [e.target.value]));
                                            })], /* Some */[false], /* Some */[true], /* None */0, /* array */[])), ReasonReact.element(/* None */0, /* None */0, MButton.make(/* None */0, /* Some */[(function () {
                                              return Curry._1(send, /* ProposePartner */0);
                                            })], /* Some */[true], /* array */[Utils.text("Propose partner addition")])), ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Body2 */-904051920, /* None */0, /* array */[Utils.text("\n               Please send the following URL to the proposed Partner so they can access the Venture:\n               ")])), ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Body2 */-904051920, /* None */0, /* array */[Utils.text(joinVentureUrl)]))), React.createElement("div", undefined, ReasonReact.element(/* None */0, /* None */0, MTypography.make(/* Body2 */-904051920, /* None */0, /* array */[Utils.text("\n               To propose the removal of a partner from this venture,\n               select his or her name below and submit your proposal.\n               When enough partners endorse this proposal, the partner will be removed.\n               ")])), ReasonReact.element(/* None */0, /* None */0, MaterialUi.List[/* make */1](/* None */0, /* None */0, /* None */0, /* Some */[true], /* None */0, /* None */0, /* array */[partners])), React.createElement("ul", undefined, partnersOld)), /* array */[]));
            }),
          /* initialState */(function () {
              return /* record */[
                      /* viewData */viewData,
                      /* prospectId */"",
                      /* currentUser */session[/* userId */0]
                    ];
            }),
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */(function (action, state) {
              if (typeof action === "number") {
                var prospectId = $$String.trim(state[/* prospectId */1]);
                if (prospectId === "") {
                  return /* NoUpdate */0;
                } else {
                  Curry._1(commands[/* proposePartner */0], PrimitiveTypes.UserId[/* fromString */1](prospectId));
                  return /* Update */Block.__(0, [/* record */[
                              /* viewData */state[/* viewData */0],
                              /* prospectId */"",
                              /* currentUser */state[/* currentUser */2]
                            ]]);
                }
              } else if (action.tag) {
                Curry._1(commands[/* proposePartnerRemoval */3], action[0]);
                return /* NoUpdate */0;
              } else {
                return /* Update */Block.__(0, [/* record */[
                            /* viewData */state[/* viewData */0],
                            /* prospectId */action[0],
                            /* currentUser */state[/* currentUser */2]
                          ]]);
              }
            }),
          /* subscriptions */component[/* subscriptions */13],
          /* jsElementWrapped */component[/* jsElementWrapped */14]
        ];
}

var text = Utils.text;

exports.text = text;
exports.changeNewPartnerId = changeNewPartnerId;
exports.component = component;
exports.make = make;
/* component Not a pure module */
