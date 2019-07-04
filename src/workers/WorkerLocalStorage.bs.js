// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var Caml_option = require("bs-platform/lib/js/caml_option.js");
var Json_decode = require("@glennsl/bs-json/src/Json_decode.bs.js");
var Json_encode = require("@glennsl/bs-json/src/Json_encode.bs.js");
var LocalStorage = require("../ffi/LocalStorage.bs.js");

function encodeItems(items) {
  return Json_encode.object_(/* :: */[
              /* tuple */[
                "blockstackSession",
                Json_encode.nullable((function (prim) {
                        return prim;
                      }), items[/* blockstackSession */0])
              ],
              /* :: */[
                /* tuple */[
                  "blockstackGaiaHubConfig",
                  Json_encode.nullable((function (prim) {
                          return prim;
                        }), items[/* blockstackGaiaHubConfig */1])
                ],
                /* :: */[
                  /* tuple */[
                    "blockstackTransitPrivateKey",
                    Json_encode.nullable((function (prim) {
                            return prim;
                          }), items[/* blockstackTransitPrivateKey */2])
                  ],
                  /* [] */0
                ]
              ]
            ]);
}

function decodeItems(raw) {
  return /* record */[
          /* blockstackSession */Json_decode.field("blockstackSession", (function (param) {
                  return Json_decode.optional(Json_decode.string, param);
                }), raw),
          /* blockstackGaiaHubConfig */Json_decode.field("blockstackGaiaHubConfig", (function (param) {
                  return Json_decode.optional(Json_decode.string, param);
                }), raw),
          /* blockstackTransitPrivateKey */Json_decode.field("blockstackTransitPrivateKey", (function (param) {
                  return Json_decode.optional(Json_decode.string, param);
                }), raw)
        ];
}

var blockstackSessionKey = "blockstack-session";

var gaiaHubKey = "blockstack-gaia-hub-config";

var transitPrivKey = "blockstack-transit-private-key";

function readBlockstackItemsFromStorage(param) {
  return /* record */[
          /* blockstackSession */Caml_option.null_to_opt(localStorage.getItem(blockstackSessionKey)),
          /* blockstackGaiaHubConfig */Caml_option.null_to_opt(localStorage.getItem(gaiaHubKey)),
          /* blockstackTransitPrivateKey */Caml_option.null_to_opt(localStorage.getItem(transitPrivKey))
        ];
}

function setBlockstackItems(param) {
  var blockstackTransitPrivateKey = param[/* blockstackTransitPrivateKey */2];
  var blockstackGaiaHubConfig = param[/* blockstackGaiaHubConfig */1];
  var blockstackSession = param[/* blockstackSession */0];
  if (blockstackSession !== undefined) {
    LocalStorage.setItem(blockstackSessionKey, blockstackSession);
  } else {
    LocalStorage.removeItem(blockstackSessionKey);
  }
  if (blockstackGaiaHubConfig !== undefined) {
    LocalStorage.setItem(gaiaHubKey, blockstackGaiaHubConfig);
  } else {
    LocalStorage.removeItem(gaiaHubKey);
  }
  if (blockstackTransitPrivateKey !== undefined) {
    return LocalStorage.setItem(transitPrivKey, blockstackTransitPrivateKey);
  } else {
    return LocalStorage.removeItem(transitPrivKey);
  }
}

var L = 0;

exports.encodeItems = encodeItems;
exports.decodeItems = decodeItems;
exports.L = L;
exports.blockstackSessionKey = blockstackSessionKey;
exports.gaiaHubKey = gaiaHubKey;
exports.transitPrivKey = transitPrivKey;
exports.readBlockstackItemsFromStorage = readBlockstackItemsFromStorage;
exports.setBlockstackItems = setBlockstackItems;
/* No side effect */
