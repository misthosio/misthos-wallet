// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var BTC = require("./BTC.bs.js");
var Block = require("bs-platform/lib/js/block.js");
var Fetch = require("bs-fetch/src/Fetch.js");
var Utils = require("../../utils/Utils.bs.js");
var Belt_Set = require("bs-platform/lib/js/belt_Set.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");
var Json_decode = require("@glennsl/bs-json/src/Json_decode.bs.js");
var WalletTypes = require("./WalletTypes.bs.js");
var BitcoinjsLib = require("bitcoinjs-lib");
var Belt_SetString = require("bs-platform/lib/js/belt_SetString.js");

(( require('formdata-polyfill') ));

var testnetConfig_001 = /* network */BitcoinjsLib.networks.testnet;

var testnetConfig = /* record */[
  /* subdomain */"testnet.",
  testnetConfig_001
];

var mainnetConfig_001 = /* network */BitcoinjsLib.networks.bitcoin;

var mainnetConfig = /* record */[
  /* subdomain */"",
  mainnetConfig_001
];

function decodeUTXO(config, raw) {
  return /* record */[
          /* txId */Json_decode.field("tx_hash_big_endian", Json_decode.string, raw),
          /* txOutputN */Json_decode.field("tx_output_n", Json_decode.$$int, raw),
          /* address */BitcoinjsLib.address.fromOutputScript(Utils.bufFromHex(Json_decode.field("script", Json_decode.string, raw)), config[/* network */1]),
          /* amount */BTC.fromSatoshisFloat(Json_decode.field("value", Json_decode.$$float, raw))
        ];
}

function getUTXOs(config, addresses) {
  if (addresses) {
    return fetch("https://" + (config[/* subdomain */0] + ("blockchain.info/unspent?format=json&active=" + (Belt_List.reduceU(addresses, "", (function (res, address) {
                                  return res + ("|" + address);
                                })) + "&cors=true")))).then((function (prim) {
                      return prim.json();
                    })).then((function (raw) {
                    return Promise.resolve(Belt_Set.mergeMany(WalletTypes.emptyUtxoSet, Json_decode.field("unspent_outputs", (function (param) {
                                          return Json_decode.array((function (param) {
                                                        return decodeUTXO(config, param);
                                                      }), param);
                                        }), raw)));
                  })).catch((function (_err) {
                  return Promise.resolve(WalletTypes.emptyUtxoSet);
                }));
  } else {
    return Promise.resolve(WalletTypes.emptyUtxoSet);
  }
}

function getTransactionInfo(config, transactions) {
  return Promise.all(Belt_List.toArray(Belt_SetString.reduceU(transactions, /* [] */0, (function (res, txId) {
                          return /* :: */[
                                  fetch("https://" + (config[/* subdomain */0] + ("blockchain.info/rawtx/" + (txId + "?format=json&cors=true")))).then((function (prim) {
                                              return prim.json();
                                            })).then((function (raw) {
                                            return Promise.resolve(/* record */[
                                                        /* txId */txId,
                                                        /* blockHeight */Json_decode.optional((function (param) {
                                                                return Json_decode.field("block_height", Json_decode.$$float, param);
                                                              }), raw),
                                                        /* unixTime */Json_decode.optional((function (param) {
                                                                return Json_decode.field("time", Json_decode.$$float, param);
                                                              }), raw)
                                                      ]);
                                          })).catch((function (param) {
                                          return Promise.resolve(undefined);
                                        })),
                                  res
                                ];
                        })))).then((function (res) {
                return Promise.resolve(Belt_List.fromArray(Belt_Array.keepMap(res, (function (res) {
                                      return res;
                                    }))));
              }));
}

function getTransactionHex(config, transactions) {
  return Promise.all(Belt_Array.mapU(transactions, (function (txId) {
                    return fetch("https://" + (config[/* subdomain */0] + ("blockchain.info/rawtx/" + (txId + "?format=hex&cors=true")))).then((function (prim) {
                                    return prim.text();
                                  })).then((function (hex) {
                                  return Promise.resolve(/* tuple */[
                                              txId,
                                              hex
                                            ]);
                                }));
                  })));
}

function getCurrentBlockHeight(config, param) {
  return fetch("https://" + (config[/* subdomain */0] + "blockchain.info/latestblock?cors=true")).then((function (prim) {
                  return prim.json();
                })).then((function (res) {
                return Promise.resolve(Json_decode.field("height", Json_decode.$$int, res));
              }));
}

function broadcastTransaction(config, transaction) {
  var txHex = transaction.toHex();
  var formData = new FormData();
  formData.append("tx", txHex);
  return fetch("https://" + (config[/* subdomain */0] + "blockchain.info/pushtx?cors=true"), Fetch.RequestInit[/* make */0](/* Post */2, undefined, Caml_option.some(formData), undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined)(/* () */0)).then((function (prim) {
                  return prim.text();
                })).then((function (responseText) {
                var responseText$1 = responseText.toLowerCase();
                var match = responseText$1.indexOf("transaction submitted") >= 0;
                var tmp;
                if (match) {
                  tmp = /* Ok */Block.__(0, [transaction.getId()]);
                } else {
                  var match$1 = responseText$1.indexOf("transaction already exists") >= 0;
                  tmp = match$1 ? /* AlreadyInBlockchain */0 : /* Error */Block.__(1, [responseText$1]);
                }
                return Promise.resolve(tmp);
              }));
}

function make(config, network) {
  var getUTXOs$1 = function (param) {
    return getUTXOs(config, param);
  };
  var getTransactionInfo$1 = function (param) {
    return getTransactionInfo(config, param);
  };
  var getTransactionHex$1 = function (param) {
    return getTransactionHex(config, param);
  };
  var getCurrentBlockHeight$1 = function (param) {
    return getCurrentBlockHeight(config, param);
  };
  var broadcastTransaction$1 = function (param) {
    return broadcastTransaction(config, param);
  };
  return /* module */[
          /* network */network,
          /* getUTXOs */getUTXOs$1,
          /* getTransactionInfo */getTransactionInfo$1,
          /* getTransactionHex */getTransactionHex$1,
          /* getCurrentBlockHeight */getCurrentBlockHeight$1,
          /* broadcastTransaction */broadcastTransaction$1
        ];
}

var float_ = Json_decode.$$float;

exports.testnetConfig = testnetConfig;
exports.mainnetConfig = mainnetConfig;
exports.float_ = float_;
exports.decodeUTXO = decodeUTXO;
exports.getUTXOs = getUTXOs;
exports.getTransactionInfo = getTransactionInfo;
exports.getTransactionHex = getTransactionHex;
exports.getCurrentBlockHeight = getCurrentBlockHeight;
exports.broadcastTransaction = broadcastTransaction;
exports.make = make;
/*  Not a pure module */
