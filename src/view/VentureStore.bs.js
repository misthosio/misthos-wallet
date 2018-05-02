// Generated by BUCKLESCRIPT VERSION 3.0.0, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Router = require("./Router.bs.js");
var ViewModel = require("./ViewModel.bs.js");
var SyncWorker = require("../SyncWorker.bs.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");
var PrimitiveTypes = require("../application/PrimitiveTypes.bs.js");
var IncomeWorkerClient = require("../workers/IncomeWorkerClient.bs.js");
var PersistWorkerClient = require("../workers/PersistWorkerClient.bs.js");
var VentureWorkerClient = require("../workers/VentureWorkerClient.bs.js");

function loadVentureAndIndex(session, currentRoute, param) {
  var ventureWorker = param[/* ventureWorker */5];
  var selectedVenture = param[/* selectedVenture */1];
  VentureWorkerClient.updateSession(ventureWorker[0]);
  if (typeof session !== "number") {
    PersistWorkerClient.updateSession(session[0][/* userId */0], param[/* persistWorker */4][0]);
  }
  if (typeof session === "number" || typeof currentRoute === "number") {
    return /* None */0;
  } else if (currentRoute.tag) {
    return /* JoiningVenture */2;
  } else {
    var ventureId = currentRoute[0];
    var exit = 0;
    if (typeof selectedVenture === "number" || selectedVenture.tag !== 1) {
      exit = 1;
    } else {
      var loadedId = selectedVenture[0];
      if (PrimitiveTypes.VentureId[/* eq */5](ventureId, loadedId)) {
        return selectedVenture;
      } else if (PrimitiveTypes.VentureId[/* neq */6](ventureId, loadedId)) {
        VentureWorkerClient.load(ventureId, ventureWorker[0]);
        return /* LoadingVenture */Block.__(0, [ventureId]);
      } else {
        exit = 1;
      }
    }
    if (exit === 1) {
      VentureWorkerClient.load(ventureId, ventureWorker[0]);
      return /* LoadingVenture */Block.__(0, [ventureId]);
    }
    
  }
}

var component = ReasonReact.reducerComponent("VentureStore");

function make(currentRoute, session, children) {
  return /* record */[
          /* debugName */component[/* debugName */0],
          /* reactClassInternal */component[/* reactClassInternal */1],
          /* handedOffState */component[/* handedOffState */2],
          /* willReceiveProps */(function (param) {
              var state = param[/* state */1];
              return /* record */[
                      /* index */state[/* index */0],
                      /* selectedVenture */loadVentureAndIndex(session, currentRoute, state),
                      /* syncWorker */state[/* syncWorker */2],
                      /* incomeWorker */state[/* incomeWorker */3],
                      /* persistWorker */state[/* persistWorker */4],
                      /* ventureWorker */state[/* ventureWorker */5]
                    ];
            }),
          /* didMount */(function (param) {
              loadVentureAndIndex(session, currentRoute, param[/* state */1]);
              return /* () */0;
            }),
          /* didUpdate */component[/* didUpdate */5],
          /* willUnmount */component[/* willUnmount */6],
          /* willUpdate */component[/* willUpdate */7],
          /* shouldUpdate */component[/* shouldUpdate */8],
          /* render */(function (param) {
              var send = param[/* send */3];
              var match = param[/* state */1];
              return Curry._3(children, match[/* index */0], match[/* selectedVenture */1], (function (name) {
                            return Curry._1(send, /* CreateVenture */Block.__(0, [name]));
                          }));
            }),
          /* initialState */(function () {
              return /* record */[
                      /* index : None */0,
                      /* selectedVenture : None */0,
                      /* syncWorker */[SyncWorker.make((function (prim) {
                                console.log(prim);
                                return /* () */0;
                              }))],
                      /* incomeWorker */[Curry._1(IncomeWorkerClient.make, (function (prim) {
                                console.log(prim);
                                return /* () */0;
                              }))],
                      /* persistWorker */[Curry._1(PersistWorkerClient.make, (function (prim) {
                                console.log(prim);
                                return /* () */0;
                              }))],
                      /* ventureWorker */[Curry._1(VentureWorkerClient.make, (function (prim) {
                                console.log(prim);
                                return /* () */0;
                              }))]
                    ];
            }),
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */(function (action, state) {
              switch (action.tag | 0) {
                case 0 : 
                    VentureWorkerClient.create(action[0], state[/* ventureWorker */5][0]);
                    return /* Update */Block.__(0, [/* record */[
                                /* index */state[/* index */0],
                                /* selectedVenture : CreatingVenture */1,
                                /* syncWorker */state[/* syncWorker */2],
                                /* incomeWorker */state[/* incomeWorker */3],
                                /* persistWorker */state[/* persistWorker */4],
                                /* ventureWorker */state[/* ventureWorker */5]
                              ]]);
                case 1 : 
                case 2 : 
                    return /* NoUpdate */0;
                case 3 : 
                    console.log("Venture '" + (PrimitiveTypes.VentureId[/* toString */0](action[0][0]) + "' persisted"));
                    return /* NoUpdate */0;
                case 4 : 
                    var msg = action[0];
                    var match = state[/* selectedVenture */1];
                    switch (msg.tag | 0) {
                      case 0 : 
                          return /* Update */Block.__(0, [/* record */[
                                      /* index : Some */[msg[0]],
                                      /* selectedVenture */state[/* selectedVenture */1],
                                      /* syncWorker */state[/* syncWorker */2],
                                      /* incomeWorker */state[/* incomeWorker */3],
                                      /* persistWorker */state[/* persistWorker */4],
                                      /* ventureWorker */state[/* ventureWorker */5]
                                    ]]);
                      case 1 : 
                          if (typeof match === "number" || match.tag) {
                            return /* NoUpdate */0;
                          } else {
                            var ventureId = msg[0];
                            if (PrimitiveTypes.VentureId[/* eq */5](ventureId, match[0])) {
                              return /* Update */Block.__(0, [/* record */[
                                          /* index */state[/* index */0],
                                          /* selectedVenture : VentureLoaded */Block.__(1, [
                                              ventureId,
                                              ViewModel.init(msg[1]),
                                              VentureWorkerClient.Cmd[/* make */0](state[/* ventureWorker */5][0], ventureId)
                                            ]),
                                          /* syncWorker */state[/* syncWorker */2],
                                          /* incomeWorker */state[/* incomeWorker */3],
                                          /* persistWorker */state[/* persistWorker */4],
                                          /* ventureWorker */state[/* ventureWorker */5]
                                        ]]);
                            } else {
                              return /* NoUpdate */0;
                            }
                          }
                      case 2 : 
                          var ventureId$1 = msg[0];
                          return /* UpdateWithSideEffects */Block.__(2, [
                                    /* record */[
                                      /* index */state[/* index */0],
                                      /* selectedVenture : VentureLoaded */Block.__(1, [
                                          ventureId$1,
                                          ViewModel.init(msg[1]),
                                          VentureWorkerClient.Cmd[/* make */0](state[/* ventureWorker */5][0], ventureId$1)
                                        ]),
                                      /* syncWorker */state[/* syncWorker */2],
                                      /* incomeWorker */state[/* incomeWorker */3],
                                      /* persistWorker */state[/* persistWorker */4],
                                      /* ventureWorker */state[/* ventureWorker */5]
                                    ],
                                    (function () {
                                        return ReasonReact.Router[/* push */0](Router.Config[/* routeToUrl */1](/* Venture */Block.__(0, [ventureId$1])));
                                      })
                                  ]);
                      case 3 : 
                          if (typeof match === "number" || match.tag !== 1) {
                            return /* NoUpdate */0;
                          } else {
                            var ventureId$2 = msg[0];
                            if (PrimitiveTypes.VentureId[/* eq */5](ventureId$2, match[0])) {
                              return /* Update */Block.__(0, [/* record */[
                                          /* index */state[/* index */0],
                                          /* selectedVenture : VentureLoaded */Block.__(1, [
                                              ventureId$2,
                                              ViewModel.applyAll(msg[1], match[1]),
                                              match[2]
                                            ]),
                                          /* syncWorker */state[/* syncWorker */2],
                                          /* incomeWorker */state[/* incomeWorker */3],
                                          /* persistWorker */state[/* persistWorker */4],
                                          /* ventureWorker */state[/* ventureWorker */5]
                                        ]]);
                            } else {
                              return /* NoUpdate */0;
                            }
                          }
                      
                    }
                
              }
            }),
          /* subscriptions */(function (param) {
              var send = param[/* send */3];
              var state = param[/* state */1];
              return /* :: */[
                      /* Sub */[
                        (function () {
                            state[/* syncWorker */2][0].terminate();
                            var worker = SyncWorker.make((function (message) {
                                    return Curry._1(send, /* SyncWorkerMessage */Block.__(1, [message]));
                                  }));
                            state[/* syncWorker */2][0] = worker;
                            return worker;
                          }),
                        (function (prim) {
                            prim.terminate();
                            return /* () */0;
                          })
                      ],
                      /* :: */[
                        /* Sub */[
                          (function () {
                              state[/* incomeWorker */3][0].terminate();
                              var worker = Curry._1(IncomeWorkerClient.make, (function (message) {
                                      return Curry._1(send, /* IncomeWorkerMessage */Block.__(2, [message]));
                                    }));
                              state[/* incomeWorker */3][0] = worker;
                              return worker;
                            }),
                          (function (prim) {
                              prim.terminate();
                              return /* () */0;
                            })
                        ],
                        /* :: */[
                          /* Sub */[
                            (function () {
                                state[/* persistWorker */4][0].terminate();
                                var worker = Curry._1(PersistWorkerClient.make, (function (message) {
                                        return Curry._1(send, /* PersistWorkerMessage */Block.__(3, [message]));
                                      }));
                                state[/* persistWorker */4][0] = worker;
                                return worker;
                              }),
                            (function (prim) {
                                prim.terminate();
                                return /* () */0;
                              })
                          ],
                          /* :: */[
                            /* Sub */[
                              (function () {
                                  state[/* ventureWorker */5][0].terminate();
                                  var worker = Curry._1(VentureWorkerClient.make, (function (message) {
                                          return Curry._1(send, /* VentureWorkerMessage */Block.__(4, [message]));
                                        }));
                                  state[/* ventureWorker */5][0] = worker;
                                  return worker;
                                }),
                              (function (prim) {
                                  prim.terminate();
                                  return /* () */0;
                                })
                            ],
                            /* [] */0
                          ]
                        ]
                      ]
                    ];
            }),
          /* jsElementWrapped */component[/* jsElementWrapped */14]
        ];
}

exports.loadVentureAndIndex = loadVentureAndIndex;
exports.component = component;
exports.make = make;
/* component Not a pure module */
