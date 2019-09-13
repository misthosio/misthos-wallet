// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var BTC = require("./BTC.bs.js");
var List = require("bs-platform/lib/js/list.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Fetch = require("bs-fetch/src/Fetch.js");
var Belt_Set = require("bs-platform/lib/js/belt_Set.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");
var Json_decode = require("@glennsl/bs-json/src/Json_decode.bs.js");
var WalletTypes = require("./WalletTypes.bs.js");
var Belt_SetString = require("bs-platform/lib/js/belt_SetString.js");

function decodeUTXO(raw) {
  return /* record */[
          /* txId */Json_decode.field("txid", Json_decode.string, raw),
          /* txOutputN */Json_decode.field("n", Json_decode.$$int, raw),
          /* address */Caml_array.caml_array_get(Json_decode.field("addresses", (function (param) {
                      return Json_decode.array(Json_decode.string, param);
                    }), raw), 0),
          /* amount */BTC.fromSatoshisFloat(Json_decode.field("value_int", Json_decode.$$float, raw))
        ];
}

function decodeUTXOs(raw) {
  return Json_decode.withDefault(/* [] */0, (function (param) {
                return Json_decode.field("unspent", (function (param) {
                              return Json_decode.withDefault(/* [] */0, (function (param) {
                                            return Json_decode.list(decodeUTXO, param);
                                          }), param);
                            }), param);
              }), raw);
}

function decodeTransaction(raw) {
  return /* record */[
          /* txId */Json_decode.field("txid", Json_decode.string, raw),
          /* blockHeight */Json_decode.field("block", (function (param) {
                  return Json_decode.optional(Json_decode.$$float, param);
                }), raw),
          /* unixTime */Json_decode.field("time", (function (param) {
                  return Json_decode.optional(Json_decode.$$float, param);
                }), raw)
        ];
}

function decodeTransactions(raw) {
  return Json_decode.withDefault(/* [] */0, (function (param) {
                return Json_decode.field("transactions", (function (param) {
                              return Json_decode.withDefault(/* [] */0, (function (param) {
                                            return Json_decode.list(decodeTransaction, param);
                                          }), param);
                            }), param);
              }), raw);
}

function decodeTransactionHexs(raw) {
  return Json_decode.withDefault(/* [] */0, (function (param) {
                return Json_decode.field("hex", (function (param) {
                              return Json_decode.withDefault(/* [] */0, (function (param) {
                                            return Json_decode.list((function (raw) {
                                                          return /* tuple */[
                                                                  Json_decode.field("txid", Json_decode.string, raw),
                                                                  Json_decode.field("hex", Json_decode.string, raw)
                                                                ];
                                                        }), param);
                                          }), param);
                            }), param);
              }), raw);
}

function decodeNextLink(raw) {
  return Json_decode.optional((function (param) {
                return Json_decode.field("paging", (function (param) {
                              return Json_decode.field("next_link", Json_decode.string, param);
                            }), param);
              }), raw);
}

function fetchAll(link, decoder, collector) {
  if (link !== undefined) {
    return fetch(link).then((function (prim) {
                    return prim.json();
                  })).then((function (res) {
                  return fetchAll(decodeNextLink(res), decoder, List.append(Curry._1(decoder, res), collector));
                }));
  } else {
    return Promise.resolve(collector);
  }
}

function getUTXOs(config, addresses) {
  if (addresses) {
    return fetchAll("https://" + (config[/* subdomain */0] + (".smartbit.com.au/v1/blockchain/address/" + (List.fold_left((function (res, a) {
                              return a + ("," + res);
                            }), "", addresses) + "/unspent?limit=1000"))), decodeUTXOs, /* [] */0).then((function (utxos) {
                  return Promise.resolve(Belt_Set.mergeMany(WalletTypes.emptyUtxoSet, Belt_List.toArray(utxos)));
                }));
  } else {
    return Promise.resolve(WalletTypes.emptyUtxoSet);
  }
}

function getTransactionInfo(config, transactions) {
  if (Belt_SetString.isEmpty(transactions)) {
    return Promise.resolve(/* [] */0);
  } else {
    return fetchAll("https://" + (config[/* subdomain */0] + (".smartbit.com.au/v1/blockchain/tx/" + Belt_SetString.reduceU(transactions, "", (function (res, a) {
                          return a + ("," + res);
                        })))), decodeTransactions, /* [] */0);
  }
}

function getTransactionHex(config, transactions) {
  if (transactions.length === 0) {
    return Promise.resolve(/* array */[]);
  } else {
    return fetchAll("https://" + (config[/* subdomain */0] + (".smartbit.com.au/v1/blockchain/tx/" + (Belt_Array.reduceU(transactions, "", (function (res, a) {
                              return a + ("," + res);
                            })) + "/hex"))), decodeTransactionHexs, /* [] */0).then((function (list_) {
                  return Promise.resolve(Belt_List.toArray(list_));
                }));
  }
}

function broadcastTransaction(config, transaction) {
  var txHex = transaction.toHex();
  return fetch("https://" + (config[/* subdomain */0] + ".smartbit.com.au/v1/blockchain/pushtx"), Fetch.RequestInit[/* make */0](/* Post */2, undefined, Caml_option.some("{\"hex\":\"" + (String(txHex) + "\"}")), undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined)(/* () */0)).then((function (prim) {
                    return prim.json();
                  })).then((function (res) {
                  var err = Json_decode.optional((function (param) {
                          return Json_decode.field("error", (function (param) {
                                        return Json_decode.field("message", Json_decode.string, param);
                                      }), param);
                        }), res);
                  var tmp;
                  if (err !== undefined) {
                    var err$1 = err;
                    tmp = (/transaction already in block chain/).test(err$1) ? /* AlreadyInBlockchain */0 : /* Error */Block.__(1, [err$1]);
                  } else {
                    tmp = /* Ok */Block.__(0, [Json_decode.field("txid", Json_decode.string, res)]);
                  }
                  return Promise.resolve(tmp);
                })).catch((function (err) {
                return Promise.resolve(/* FetchError */Block.__(2, [err]));
              }));
}

function getCurrentBlockHeight(config, param) {
  return fetch("https://" + (config[/* subdomain */0] + ".smartbit.com.au/v1/blockchain/blocks?sort=height")).then((function (prim) {
                  return prim.json();
                })).then((function (res) {
                return Promise.resolve(Belt_Array.getExn(Json_decode.field("blocks", (function (param) {
                                      return Json_decode.array((function (block) {
                                                    return Json_decode.field("height", Json_decode.$$int, block);
                                                  }), param);
                                    }), res), 0));
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

var testnetConfig = /* record */[/* subdomain */"testnet-api"];

var mainnetConfig = /* record */[/* subdomain */"api"];

var float_ = Json_decode.$$float;

exports.testnetConfig = testnetConfig;
exports.mainnetConfig = mainnetConfig;
exports.float_ = float_;
exports.decodeUTXO = decodeUTXO;
exports.decodeUTXOs = decodeUTXOs;
exports.decodeTransaction = decodeTransaction;
exports.decodeTransactions = decodeTransactions;
exports.decodeTransactionHexs = decodeTransactionHexs;
exports.decodeNextLink = decodeNextLink;
exports.fetchAll = fetchAll;
exports.getUTXOs = getUTXOs;
exports.getTransactionInfo = getTransactionInfo;
exports.getTransactionHex = getTransactionHex;
exports.broadcastTransaction = broadcastTransaction;
exports.getCurrentBlockHeight = getCurrentBlockHeight;
exports.make = make;
/* BTC Not a pure module */
