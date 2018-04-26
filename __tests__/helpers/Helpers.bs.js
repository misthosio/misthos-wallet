// Generated by BUCKLESCRIPT VERSION 2.2.3, PLEASE EDIT WITH CARE
'use strict';

var BTC = require("../../src/application/wallet/BTC.bs.js");
var List = require("bs-platform/lib/js/list.js");
var Js_exn = require("bs-platform/lib/js/js_exn.js");
var $$String = require("bs-platform/lib/js/string.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");
var BitcoinjsLib = require("bitcoinjs-lib");
var Child_process = require("child_process");
var BitcoindClient = require("../../src/application/wallet/BitcoindClient.bs.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");

function enableHttpRequests() {
  return ( global.XMLHttpRequest = require('xmlhttprequest').XMLHttpRequest );
}

var faucetKey = BitcoinjsLib.ECPair.fromWIF("92Qba5hnyWSn5Ffcka56yMQauaWY6ZLd91Vzxbi4a9CCetaHtYj", BitcoinjsLib.networks.testnet);

var faucetAddress = faucetKey.getAddress();

var bitcoindConfig = /* record */[
  /* bitcoindUrl */"http://localhost:18322",
  /* rpcUser */"bitcoin",
  /* rpcPassword */"bitcoin"
];

var defaultFee = BTC.fromSatoshis(/* int64 */[
      /* hi */0,
      /* lo */1000
    ]);

function selectUTXOs(utxos, totalAmount) {
  var utxos$1 = List.sort((function (u1, u2) {
          return u1[/* amount */3].comparedTo(u2[/* amount */3]);
        }), List.filter((function (param) {
                return +(param[/* confirmations */4] > 0);
              }))(utxos));
  return List.fold_left((function (param, utxo) {
                var total = param[1];
                var result = param[0];
                if (total.gt(totalAmount.plus(defaultFee))) {
                  return /* tuple */[
                          result,
                          total
                        ];
                } else {
                  return /* tuple */[
                          /* :: */[
                            utxo,
                            result
                          ],
                          total.plus(utxo[/* amount */3])
                        ];
                }
              }), /* tuple */[
              /* [] */0,
              BTC.zero
            ], utxos$1);
}

var FaucetEmpty = Caml_exceptions.create("Helpers.FaucetEmpty");

function getUTXOs(param) {
  return BitcoindClient.getUTXOs(bitcoindConfig, param);
}

function broadcastTransaction(tx) {
  return BitcoindClient.broadcastTransaction(bitcoindConfig, tx).then((function (result) {
                Child_process.execSync("bitcoin-cli -regtest -rpcuser=bitcoin -rpcpassword=bitcoin -rpcport=18322 generate 2", {
                      encoding: "utf8"
                    });
                if (typeof result === "number") {
                  console.log(result);
                  return Js_exn.raiseError("helper transaction failed");
                } else if (result.tag) {
                  console.log(result);
                  return Js_exn.raiseError("helper transaction failed");
                } else {
                  return Promise.resolve(result[0]);
                }
              }));
}

function fundAddress(outputs, utxos) {
  var totalValues = List.fold_left((function (n, v) {
          return n.plus(v);
        }), BTC.zero, List.map((function (prim) {
              return prim[1];
            }), outputs));
  var match = selectUTXOs(utxos, totalValues);
  var totalIn = match[1];
  var inputs = match[0];
  if (Caml_obj.caml_lessthan(totalIn, totalValues)) {
    throw FaucetEmpty;
  }
  var txB = new BitcoinjsLib.TransactionBuilder(BitcoinjsLib.networks.testnet);
  List.iter((function (utxo) {
          txB.addInput(utxo[/* txId */0], utxo[/* txOutputN */1]);
          return /* () */0;
        }), inputs);
  List.iter((function (param) {
          txB.addOutput(param[0], BTC.toSatoshisFloat(param[1]));
          return /* () */0;
        }), outputs);
  var remainder = totalIn.minus(totalValues).minus(defaultFee);
  txB.addOutput(faucetKey.getAddress(), BTC.toSatoshisFloat(remainder));
  List.iteri((function (i, _) {
          txB.sign(i, faucetKey);
          return /* () */0;
        }), inputs);
  return broadcastTransaction(txB.build()).then((function () {
                  return BitcoindClient.getUTXOs(bitcoindConfig, List.map((function (prim) {
                                    return prim[0];
                                  }), outputs));
                })).then((function (utxos) {
                return Promise.resolve(List.map((function (utxo) {
                                  return /* record */[
                                          /* txId */utxo[/* txId */0],
                                          /* outputs : :: */[
                                            /* record */[
                                              /* address */utxo[/* address */2],
                                              /* amount */utxo[/* amount */3]
                                            ],
                                            /* [] */0
                                          ]
                                        ];
                                }), utxos));
              }));
}

function faucet(outputs) {
  return BitcoindClient.getUTXOs(bitcoindConfig, /* :: */[
                faucetKey.getAddress(),
                /* [] */0
              ]).then((function (param) {
                return fundAddress(outputs, param);
              }));
}

function rangeTo(x, y) {
  if (x === y) {
    return /* [] */0;
  } else {
    return /* :: */[
            x,
            rangeTo(x + 1 | 0, y)
          ];
  }
}

function displayTx(txHex) {
  var output = "Version:       " + $$String.sub(txHex, 0, 8);
  var output$1 = output + ("\nSegwit Marker: " + $$String.sub(txHex, 8, 2));
  var segwitFlagPos = 10;
  var output$2 = output$1 + ("\nSegwit Marker: " + $$String.sub(txHex, segwitFlagPos, 2));
  var nInputsPos = segwitFlagPos + 2 | 0;
  var output$3 = output$2 + ("\nNum Inputs:    " + $$String.sub(txHex, nInputsPos, 2));
  var nInputs = Caml_format.caml_int_of_string("0x" + $$String.sub(txHex, nInputsPos, 2));
  var match = List.fold_left((function (param, _) {
          var pos = param[0];
          var output = param[1] + ("\nPrevOutHash:   " + $$String.sub(txHex, pos, 64));
          var prevOutputIndexPos = pos + 64 | 0;
          var output$1 = output + ("\nPrevOutIndex:  " + $$String.sub(txHex, prevOutputIndexPos, 8));
          var scriptLengthPos = prevOutputIndexPos + 8 | 0;
          var output$2 = output$1 + ("\nScriptLength:  " + $$String.sub(txHex, scriptLengthPos, 2));
          var scriptPos = scriptLengthPos + 2 | 0;
          var scriptLength = (Caml_format.caml_int_of_string("0x" + $$String.sub(txHex, scriptLengthPos, 2)) << 1);
          var output$3 = output$2 + ("\nScript (" + (String(scriptLength / 2 | 0) + ("):   " + $$String.sub(txHex, scriptPos, scriptLength))));
          var sequencePos = scriptPos + scriptLength | 0;
          var output$4 = output$3 + ("\nScriptLength:  " + $$String.sub(txHex, sequencePos, 8));
          return /* tuple */[
                  sequencePos + 8 | 0,
                  output$4
                ];
        }), /* tuple */[
        nInputsPos + 2 | 0,
        ""
      ], rangeTo(0, nInputs));
  var pos = match[0];
  var output$4 = output$3 + match[1];
  var output$5 = output$4 + ("\nOutputCount:   " + $$String.sub(txHex, pos, 2));
  var outputCount = Caml_format.caml_int_of_string("0x" + $$String.sub(txHex, pos, 2));
  var match$1 = List.fold_left((function (param, _) {
          var pos = param[0];
          var output = param[1] + ("\nOutputVal:     " + $$String.sub(txHex, pos, 16));
          var scriptLengthPos = pos + 16 | 0;
          var output$1 = output + ("\nScriptLength:  " + $$String.sub(txHex, scriptLengthPos, 2));
          var scriptPos = scriptLengthPos + 2 | 0;
          var scriptLength = (Caml_format.caml_int_of_string("0x" + $$String.sub(txHex, scriptLengthPos, 2)) << 1);
          var output$2 = output$1 + ("\nScript (" + (String(scriptLength / 2 | 0) + ("):   " + $$String.sub(txHex, scriptPos, scriptLength))));
          return /* tuple */[
                  scriptPos + scriptLength | 0,
                  output$2
                ];
        }), /* tuple */[
        pos + 2 | 0,
        ""
      ], rangeTo(0, outputCount));
  var output$6 = output$5 + match$1[1];
  var match$2 = List.fold_left((function (param, _) {
          var pos = param[0];
          var output = param[1] + ("\nNumStackItems:  " + $$String.sub(txHex, pos, 2));
          var numberOfStackItems = Caml_format.caml_int_of_string("0x" + $$String.sub(txHex, pos, 2));
          var match = List.fold_left((function (param, _) {
                  var pos = param[0];
                  var output = param[1] + ("\nStackSizeOfItem:  " + $$String.sub(txHex, pos, 2));
                  var stackItemPos = pos + 2 | 0;
                  var stackItemLength = (Caml_format.caml_int_of_string("0x" + $$String.sub(txHex, pos, 2)) << 1);
                  var output$1 = output + ("\nStackItem (" + (String(stackItemLength / 2 | 0) + ("):   " + $$String.sub(txHex, stackItemPos, stackItemLength))));
                  return /* tuple */[
                          stackItemPos + stackItemLength | 0,
                          output$1
                        ];
                }), /* tuple */[
                pos + 2 | 0,
                ""
              ], rangeTo(0, numberOfStackItems));
          var output$1 = output + match[1];
          return /* tuple */[
                  match[0],
                  output$1
                ];
        }), /* tuple */[
        match$1[0],
        ""
      ], rangeTo(0, nInputs));
  var pos$1 = match$2[0];
  var output$7 = output$6 + match$2[1];
  var output$8 = output$7 + ("\nLocktime:     " + $$String.sub(txHex, pos$1, 8));
  console.log(output$8);
  console.log(pos$1 + 8 | 0);
  console.log(txHex.length);
  return /* () */0;
}

exports.enableHttpRequests = enableHttpRequests;
exports.faucetKey = faucetKey;
exports.faucetAddress = faucetAddress;
exports.bitcoindConfig = bitcoindConfig;
exports.defaultFee = defaultFee;
exports.selectUTXOs = selectUTXOs;
exports.FaucetEmpty = FaucetEmpty;
exports.getUTXOs = getUTXOs;
exports.broadcastTransaction = broadcastTransaction;
exports.fundAddress = fundAddress;
exports.faucet = faucet;
exports.rangeTo = rangeTo;
exports.displayTx = displayTx;
/* faucetKey Not a pure module */