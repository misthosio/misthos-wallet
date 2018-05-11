// Generated by BUCKLESCRIPT VERSION 3.0.0, PLEASE EDIT WITH CARE
'use strict';

var BTC = require("../application/wallet/BTC.bs.js");
var Block = require("bs-platform/lib/js/block.js");
var Event = require("../application/events/Event.bs.js");
var Venture = require("../application/Venture.bs.js");
var EventLog = require("../application/events/EventLog.bs.js");
var Json_decode = require("bs-json/src/Json_decode.js");
var Json_encode = require("bs-json/src/Json_encode.js");
var WalletTypes = require("../application/wallet/WalletTypes.bs.js");
var PrimitiveTypes = require("../application/PrimitiveTypes.bs.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");
var WorkerLocalStorage = require("./WorkerLocalStorage.bs.js");

var UnknownMessage = Caml_exceptions.create("VentureWorkerMessage.UnknownMessage");

function encodeIncoming(param) {
  switch (param.tag | 0) {
    case 0 : 
        return Json_encode.object_(/* :: */[
                    /* tuple */[
                      "type",
                      "UpdateSession"
                    ],
                    /* :: */[
                      /* tuple */[
                        "blockstackItems",
                        WorkerLocalStorage.encodeItems(param[0])
                      ],
                      /* [] */0
                    ]
                  ]);
    case 1 : 
        return Json_encode.object_(/* :: */[
                    /* tuple */[
                      "type",
                      "Create"
                    ],
                    /* :: */[
                      /* tuple */[
                        "name",
                        param[0]
                      ],
                      /* [] */0
                    ]
                  ]);
    case 2 : 
        return Json_encode.object_(/* :: */[
                    /* tuple */[
                      "type",
                      "Load"
                    ],
                    /* :: */[
                      /* tuple */[
                        "ventureId",
                        PrimitiveTypes.VentureId[/* encode */2](param[0])
                      ],
                      /* [] */0
                    ]
                  ]);
    case 3 : 
        return Json_encode.object_(/* :: */[
                    /* tuple */[
                      "type",
                      "JoinVia"
                    ],
                    /* :: */[
                      /* tuple */[
                        "ventureId",
                        PrimitiveTypes.VentureId[/* encode */2](param[0])
                      ],
                      /* :: */[
                        /* tuple */[
                          "userId",
                          PrimitiveTypes.UserId[/* encode */2](param[1])
                        ],
                        /* [] */0
                      ]
                    ]
                  ]);
    case 4 : 
        return Json_encode.object_(/* :: */[
                    /* tuple */[
                      "type",
                      "ProposePartner"
                    ],
                    /* :: */[
                      /* tuple */[
                        "ventureId",
                        PrimitiveTypes.VentureId[/* encode */2](param[0])
                      ],
                      /* :: */[
                        /* tuple */[
                          "userId",
                          PrimitiveTypes.UserId[/* encode */2](param[1])
                        ],
                        /* [] */0
                      ]
                    ]
                  ]);
    case 5 : 
        return Json_encode.object_(/* :: */[
                    /* tuple */[
                      "type",
                      "RejectPartner"
                    ],
                    /* :: */[
                      /* tuple */[
                        "ventureId",
                        PrimitiveTypes.VentureId[/* encode */2](param[0])
                      ],
                      /* :: */[
                        /* tuple */[
                          "processId",
                          PrimitiveTypes.ProcessId[/* encode */2](param[1])
                        ],
                        /* [] */0
                      ]
                    ]
                  ]);
    case 6 : 
        return Json_encode.object_(/* :: */[
                    /* tuple */[
                      "type",
                      "EndorsePartner"
                    ],
                    /* :: */[
                      /* tuple */[
                        "ventureId",
                        PrimitiveTypes.VentureId[/* encode */2](param[0])
                      ],
                      /* :: */[
                        /* tuple */[
                          "processId",
                          PrimitiveTypes.ProcessId[/* encode */2](param[1])
                        ],
                        /* [] */0
                      ]
                    ]
                  ]);
    case 7 : 
        return Json_encode.object_(/* :: */[
                    /* tuple */[
                      "type",
                      "ProposePartnerRemoval"
                    ],
                    /* :: */[
                      /* tuple */[
                        "ventureId",
                        PrimitiveTypes.VentureId[/* encode */2](param[0])
                      ],
                      /* :: */[
                        /* tuple */[
                          "userId",
                          PrimitiveTypes.UserId[/* encode */2](param[1])
                        ],
                        /* [] */0
                      ]
                    ]
                  ]);
    case 8 : 
        return Json_encode.object_(/* :: */[
                    /* tuple */[
                      "type",
                      "RejectPartnerRemoval"
                    ],
                    /* :: */[
                      /* tuple */[
                        "ventureId",
                        PrimitiveTypes.VentureId[/* encode */2](param[0])
                      ],
                      /* :: */[
                        /* tuple */[
                          "processId",
                          PrimitiveTypes.ProcessId[/* encode */2](param[1])
                        ],
                        /* [] */0
                      ]
                    ]
                  ]);
    case 9 : 
        return Json_encode.object_(/* :: */[
                    /* tuple */[
                      "type",
                      "EndorsePartnerRemoval"
                    ],
                    /* :: */[
                      /* tuple */[
                        "ventureId",
                        PrimitiveTypes.VentureId[/* encode */2](param[0])
                      ],
                      /* :: */[
                        /* tuple */[
                          "processId",
                          PrimitiveTypes.ProcessId[/* encode */2](param[1])
                        ],
                        /* [] */0
                      ]
                    ]
                  ]);
    case 10 : 
        return Json_encode.object_(/* :: */[
                    /* tuple */[
                      "type",
                      "ProposePayout"
                    ],
                    /* :: */[
                      /* tuple */[
                        "ventureId",
                        PrimitiveTypes.VentureId[/* encode */2](param[0])
                      ],
                      /* :: */[
                        /* tuple */[
                          "accountIdx",
                          WalletTypes.AccountIndex[/* encode */4](param[1])
                        ],
                        /* :: */[
                          /* tuple */[
                            "destinations",
                            Json_encode.list((function (param) {
                                    return Json_encode.tuple2((function (prim) {
                                                  return prim;
                                                }), BTC.encode, param);
                                  }), param[2])
                          ],
                          /* :: */[
                            /* tuple */[
                              "fee",
                              BTC.encode(param[3])
                            ],
                            /* [] */0
                          ]
                        ]
                      ]
                    ]
                  ]);
    case 11 : 
        return Json_encode.object_(/* :: */[
                    /* tuple */[
                      "type",
                      "RejectPayout"
                    ],
                    /* :: */[
                      /* tuple */[
                        "ventureId",
                        PrimitiveTypes.VentureId[/* encode */2](param[0])
                      ],
                      /* :: */[
                        /* tuple */[
                          "processId",
                          PrimitiveTypes.ProcessId[/* encode */2](param[1])
                        ],
                        /* [] */0
                      ]
                    ]
                  ]);
    case 12 : 
        return Json_encode.object_(/* :: */[
                    /* tuple */[
                      "type",
                      "EndorsePayout"
                    ],
                    /* :: */[
                      /* tuple */[
                        "ventureId",
                        PrimitiveTypes.VentureId[/* encode */2](param[0])
                      ],
                      /* :: */[
                        /* tuple */[
                          "processId",
                          PrimitiveTypes.ProcessId[/* encode */2](param[1])
                        ],
                        /* [] */0
                      ]
                    ]
                  ]);
    case 13 : 
        return Json_encode.object_(/* :: */[
                    /* tuple */[
                      "type",
                      "ExposeIncomeAddress"
                    ],
                    /* :: */[
                      /* tuple */[
                        "ventureId",
                        PrimitiveTypes.VentureId[/* encode */2](param[0])
                      ],
                      /* :: */[
                        /* tuple */[
                          "accountIdx",
                          WalletTypes.AccountIndex[/* encode */4](param[1])
                        ],
                        /* [] */0
                      ]
                    ]
                  ]);
    case 14 : 
        return Json_encode.object_(/* :: */[
                    /* tuple */[
                      "type",
                      "TransactionDetected"
                    ],
                    /* :: */[
                      /* tuple */[
                        "ventureId",
                        PrimitiveTypes.VentureId[/* encode */2](param[0])
                      ],
                      /* :: */[
                        /* tuple */[
                          "incomeEvents",
                          Json_encode.list(Event.IncomeDetected[/* encode */1], param[1])
                        ],
                        /* [] */0
                      ]
                    ]
                  ]);
    case 15 : 
        return Json_encode.object_(/* :: */[
                    /* tuple */[
                      "type",
                      "NewItemsDetected"
                    ],
                    /* :: */[
                      /* tuple */[
                        "ventureId",
                        PrimitiveTypes.VentureId[/* encode */2](param[0])
                      ],
                      /* :: */[
                        /* tuple */[
                          "items",
                          Json_encode.list(EventLog.encodeItem, param[1])
                        ],
                        /* [] */0
                      ]
                    ]
                  ]);
    
  }
}

function decodeIncoming(raw) {
  var type_ = Json_decode.field("type", Json_decode.string, raw);
  switch (type_) {
    case "Create" : 
        var name = Json_decode.field("name", Json_decode.string, raw);
        return /* Create */Block.__(1, [name]);
    case "EndorsePartner" : 
        var ventureId = Json_decode.field("ventureId", PrimitiveTypes.VentureId[/* decode */3], raw);
        var processId = Json_decode.field("processId", PrimitiveTypes.ProcessId[/* decode */3], raw);
        return /* EndorsePartner */Block.__(6, [
                  ventureId,
                  processId
                ]);
    case "EndorsePartnerRemoval" : 
        var ventureId$1 = Json_decode.field("ventureId", PrimitiveTypes.VentureId[/* decode */3], raw);
        var processId$1 = Json_decode.field("processId", PrimitiveTypes.ProcessId[/* decode */3], raw);
        return /* EndorsePartnerRemoval */Block.__(9, [
                  ventureId$1,
                  processId$1
                ]);
    case "EndorsePayout" : 
        var ventureId$2 = Json_decode.field("ventureId", PrimitiveTypes.VentureId[/* decode */3], raw);
        var processId$2 = Json_decode.field("processId", PrimitiveTypes.ProcessId[/* decode */3], raw);
        return /* EndorsePayout */Block.__(12, [
                  ventureId$2,
                  processId$2
                ]);
    case "ExposeIncomeAddress" : 
        var ventureId$3 = Json_decode.field("ventureId", PrimitiveTypes.VentureId[/* decode */3], raw);
        var accountIdx = Json_decode.field("accountIdx", WalletTypes.AccountIndex[/* decode */5], raw);
        return /* ExposeIncomeAddress */Block.__(13, [
                  ventureId$3,
                  accountIdx
                ]);
    case "JoinVia" : 
        var ventureId$4 = Json_decode.field("ventureId", PrimitiveTypes.VentureId[/* decode */3], raw);
        var userId = Json_decode.field("userId", PrimitiveTypes.UserId[/* decode */3], raw);
        return /* JoinVia */Block.__(3, [
                  ventureId$4,
                  userId
                ]);
    case "Load" : 
        var ventureId$5 = Json_decode.field("ventureId", PrimitiveTypes.VentureId[/* decode */3], raw);
        return /* Load */Block.__(2, [ventureId$5]);
    case "NewItemsDetected" : 
        var ventureId$6 = Json_decode.field("ventureId", PrimitiveTypes.VentureId[/* decode */3], raw);
        var items = Json_decode.field("items", (function (param) {
                return Json_decode.list(EventLog.decodeItem, param);
              }), raw);
        return /* NewItemsDetected */Block.__(15, [
                  ventureId$6,
                  items
                ]);
    case "ProposePartner" : 
        var ventureId$7 = Json_decode.field("ventureId", PrimitiveTypes.VentureId[/* decode */3], raw);
        var userId$1 = Json_decode.field("userId", PrimitiveTypes.UserId[/* decode */3], raw);
        return /* ProposePartner */Block.__(4, [
                  ventureId$7,
                  userId$1
                ]);
    case "ProposePartnerRemoval" : 
        var ventureId$8 = Json_decode.field("ventureId", PrimitiveTypes.VentureId[/* decode */3], raw);
        var userId$2 = Json_decode.field("userId", PrimitiveTypes.UserId[/* decode */3], raw);
        return /* ProposePartnerRemoval */Block.__(7, [
                  ventureId$8,
                  userId$2
                ]);
    case "ProposePayout" : 
        var ventureId$9 = Json_decode.field("ventureId", PrimitiveTypes.VentureId[/* decode */3], raw);
        var accountIdx$1 = Json_decode.field("accountIdx", WalletTypes.AccountIndex[/* decode */5], raw);
        var destinations = Json_decode.field("destinations", (function (param) {
                return Json_decode.list((function (param) {
                              return Json_decode.tuple2(Json_decode.string, BTC.decode, param);
                            }), param);
              }), raw);
        var fee = Json_decode.field("fee", BTC.decode, raw);
        return /* ProposePayout */Block.__(10, [
                  ventureId$9,
                  accountIdx$1,
                  destinations,
                  fee
                ]);
    case "RejectPartner" : 
        var ventureId$10 = Json_decode.field("ventureId", PrimitiveTypes.VentureId[/* decode */3], raw);
        var processId$3 = Json_decode.field("processId", PrimitiveTypes.ProcessId[/* decode */3], raw);
        return /* RejectPartner */Block.__(5, [
                  ventureId$10,
                  processId$3
                ]);
    case "RejectPartnerRemoval" : 
        var ventureId$11 = Json_decode.field("ventureId", PrimitiveTypes.VentureId[/* decode */3], raw);
        var processId$4 = Json_decode.field("processId", PrimitiveTypes.ProcessId[/* decode */3], raw);
        return /* RejectPartnerRemoval */Block.__(8, [
                  ventureId$11,
                  processId$4
                ]);
    case "RejectPayout" : 
        var ventureId$12 = Json_decode.field("ventureId", PrimitiveTypes.VentureId[/* decode */3], raw);
        var processId$5 = Json_decode.field("processId", PrimitiveTypes.ProcessId[/* decode */3], raw);
        return /* RejectPayout */Block.__(11, [
                  ventureId$12,
                  processId$5
                ]);
    case "TransactionDetected" : 
        var ventureId$13 = Json_decode.field("ventureId", PrimitiveTypes.VentureId[/* decode */3], raw);
        var partial_arg = Event.IncomeDetected[/* decode */2];
        var incomeEvents = Json_decode.field("incomeEvents", (function (param) {
                return Json_decode.list(partial_arg, param);
              }), raw);
        return /* TransactionDetected */Block.__(14, [
                  ventureId$13,
                  incomeEvents
                ]);
    case "UpdateSession" : 
        var blockstackItems = Json_decode.field("blockstackItems", WorkerLocalStorage.decodeItems, raw);
        return /* UpdateSession */Block.__(0, [blockstackItems]);
    default:
      throw [
            UnknownMessage,
            raw
          ];
  }
}

function encodeOutgoing(param) {
  switch (param.tag | 0) {
    case 0 : 
        return Json_encode.object_(/* :: */[
                    /* tuple */[
                      "type",
                      "UpdateIndex"
                    ],
                    /* :: */[
                      /* tuple */[
                        "index",
                        Venture.Index[/* encode */1](param[0])
                      ],
                      /* [] */0
                    ]
                  ]);
    case 1 : 
        return Json_encode.object_(/* :: */[
                    /* tuple */[
                      "type",
                      "VentureLoaded"
                    ],
                    /* :: */[
                      /* tuple */[
                        "ventureId",
                        PrimitiveTypes.VentureId[/* encode */2](param[0])
                      ],
                      /* :: */[
                        /* tuple */[
                          "events",
                          Json_encode.list(Event.encode, param[1])
                        ],
                        /* [] */0
                      ]
                    ]
                  ]);
    case 2 : 
        return Json_encode.object_(/* :: */[
                    /* tuple */[
                      "type",
                      "VentureCreated"
                    ],
                    /* :: */[
                      /* tuple */[
                        "ventureId",
                        PrimitiveTypes.VentureId[/* encode */2](param[0])
                      ],
                      /* :: */[
                        /* tuple */[
                          "events",
                          Json_encode.list(Event.encode, param[1])
                        ],
                        /* [] */0
                      ]
                    ]
                  ]);
    case 3 : 
        return Json_encode.object_(/* :: */[
                    /* tuple */[
                      "type",
                      "NewItems"
                    ],
                    /* :: */[
                      /* tuple */[
                        "ventureId",
                        PrimitiveTypes.VentureId[/* encode */2](param[0])
                      ],
                      /* :: */[
                        /* tuple */[
                          "items",
                          Json_encode.list(EventLog.encodeItem, param[1])
                        ],
                        /* [] */0
                      ]
                    ]
                  ]);
    
  }
}

function decodeOutgoing(raw) {
  var type_ = Json_decode.field("type", Json_decode.string, raw);
  switch (type_) {
    case "NewItems" : 
        var ventureId = Json_decode.field("ventureId", PrimitiveTypes.VentureId[/* decode */3], raw);
        var items = Json_decode.field("items", (function (param) {
                return Json_decode.list(EventLog.decodeItem, param);
              }), raw);
        return /* NewItems */Block.__(3, [
                  ventureId,
                  items
                ]);
    case "UpdateIndex" : 
        return /* UpdateIndex */Block.__(0, [Json_decode.field("index", Venture.Index[/* decode */2], raw)]);
    case "VentureCreated" : 
        var ventureId$1 = Json_decode.field("ventureId", PrimitiveTypes.VentureId[/* decode */3], raw);
        var events = Json_decode.field("events", (function (param) {
                return Json_decode.list(Event.decode, param);
              }), raw);
        return /* VentureCreated */Block.__(2, [
                  ventureId$1,
                  events
                ]);
    case "VentureLoaded" : 
        var ventureId$2 = Json_decode.field("ventureId", PrimitiveTypes.VentureId[/* decode */3], raw);
        var events$1 = Json_decode.field("events", (function (param) {
                return Json_decode.list(Event.decode, param);
              }), raw);
        return /* VentureLoaded */Block.__(1, [
                  ventureId$2,
                  events$1
                ]);
    default:
      throw [
            UnknownMessage,
            raw
          ];
  }
}

exports.UnknownMessage = UnknownMessage;
exports.encodeIncoming = encodeIncoming;
exports.decodeIncoming = decodeIncoming;
exports.encodeOutgoing = encodeOutgoing;
exports.decodeOutgoing = decodeOutgoing;
/* BTC Not a pure module */
