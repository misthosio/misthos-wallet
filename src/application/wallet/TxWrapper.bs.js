// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var BTC = require("./BTC.bs.js");
var Utils = require("../../utils/Utils.bs.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Js_option = require("bs-platform/lib/js/js_option.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");
var BitcoinjsLib = require("bitcoinjs-lib");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");

function extractInputs(tx) {
  return Belt_Array.map(tx.ins, (function (input) {
                var witness = input.witness;
                var arg = witness.length - 2 | 0;
                var witness$1 = (function (param) {
                      return Belt_Array.slice(param, 1, arg);
                    })(witness);
                return /* record */[
                        /* signatures */witness$1,
                        /* sequence */input.sequence
                      ];
              }));
}

function make(hex) {
  var tx = BitcoinjsLib.Transaction.fromHex(hex);
  return /* record */[
          /* tx */tx,
          /* inputs */extractInputs(tx)
        ];
}

function needsSigning(idx, nCoSigners, param) {
  var input = Belt_Array.getExn(param[/* inputs */1], idx);
  var sigs = input[/* signatures */0];
  if (sigs.length !== 0) {
    return Belt_Array.reduceU(sigs, 0, (function (res, sig_) {
                  var match = BitcoinjsLib.script.isCanonicalScriptSignature(sig_);
                  if (match) {
                    return res + 1 | 0;
                  } else {
                    return res;
                  }
                })) < nCoSigners;
  } else {
    return true;
  }
}

function pubKeyIndex(witnessBuf, nCustodians, pubKey) {
  var chunks = BitcoinjsLib.script.decompile(witnessBuf);
  var pubKeys = Belt_Array.slice(chunks, (chunks.length - 2 | 0) - nCustodians | 0, nCustodians);
  return Belt_Array.reduceU(pubKeys, /* tuple */[
                -1,
                0
              ], (function (param, key) {
                  var idx = param[1];
                  var match = Buffer.compare(key, pubKey) === 0;
                  if (match) {
                    return /* tuple */[
                            idx,
                            idx + 1 | 0
                          ];
                  } else {
                    return /* tuple */[
                            param[0],
                            idx + 1 | 0
                          ];
                  }
                }))[0];
}

function sign(idx, keyPair, nCustodians, redeemScript, witnessValue, witnessScript, signature, param) {
  var tx = param[/* tx */0];
  var witnessBuf = Utils.bufFromHex(witnessScript);
  tx.setInputScript(idx, BitcoinjsLib.script.compile(/* array */[Utils.bufFromHex(redeemScript)]));
  var match;
  if (signature !== undefined) {
    var match$1 = signature;
    match = /* tuple */[
      Utils.bufFromHex(match$1[0]),
      Utils.bufFromHex(match$1[1])
    ];
  } else {
    var signatureHash = tx.hashForWitnessV0(idx, witnessBuf, BTC.toSatoshisFloat(witnessValue), BitcoinjsLib.Transaction.SIGHASH_ALL);
    match = /* tuple */[
      keyPair.publicKey,
      BitcoinjsLib.script.signature.encode(keyPair.sign(signatureHash), BitcoinjsLib.Transaction.SIGHASH_ALL)
    ];
  }
  var insert = pubKeyIndex(witnessBuf, nCustodians, match[0]);
  var input = Belt_Array.getExn(param[/* inputs */1], idx);
  var sigs = input[/* signatures */0];
  var signatures = sigs.length !== 0 ? sigs : Belt_Array.makeByU(nCustodians, (function (param) {
            return Buffer.alloc(0);
          }));
  Belt_Array.set(signatures, insert, match[1]);
  tx.setWitness(idx, Belt_Array.concatMany(/* array */[
            /* array */[Buffer.alloc(0)],
            signatures,
            /* array */[witnessBuf]
          ]));
  return /* record */[
          /* tx */tx,
          /* inputs */extractInputs(tx)
        ];
}

function getWitnessBuf(idx, tx) {
  var ins = tx.ins;
  var witnessScript = Belt_Array.getExn(ins, idx).witness;
  return Belt_Array.get(witnessScript, witnessScript.length - 1 | 0);
}

function merge(param, param$1) {
  var otherInputs = param$1[/* inputs */1];
  var otherTx = param$1[/* tx */0];
  var tx = param[/* tx */0];
  Belt_Array.forEachWithIndexU(param[/* inputs */1], (function (idx, param) {
          var signatures = param[/* signatures */0];
          var otherSigs = Belt_Array.getExn(otherInputs, idx)[/* signatures */0];
          var signatures$1 = signatures.length !== 0 ? (
              otherSigs.length !== 0 ? Belt_List.toArray(Belt_Array.reduceReverse2U(signatures, otherSigs, /* [] */0, (function (res, sigA, sigB) {
                            var match = BitcoinjsLib.script.isCanonicalScriptSignature(sigA);
                            return /* :: */[
                                    match ? sigA : sigB,
                                    res
                                  ];
                          }))) : signatures
            ) : otherSigs;
          var match = getWitnessBuf(idx, tx);
          var match$1 = getWitnessBuf(idx, otherTx);
          if (match !== undefined) {
            tx.setWitness(idx, Belt_Array.concatMany(/* array */[
                      /* array */[Buffer.alloc(0)],
                      signatures$1,
                      /* array */[Caml_option.valFromOption(match)]
                    ]));
            return /* () */0;
          } else if (match$1 !== undefined) {
            var txInputs = otherTx.ins;
            var txIn = Belt_Array.getExn(txInputs, idx);
            tx.setInputScript(idx, txIn.script);
            tx.setWitness(idx, Belt_Array.concatMany(/* array */[
                      /* array */[Buffer.alloc(0)],
                      signatures$1,
                      /* array */[Caml_option.valFromOption(match$1)]
                    ]));
            return /* () */0;
          } else {
            return /* () */0;
          }
        }));
  return /* record */[
          /* tx */tx,
          /* inputs */extractInputs(tx)
        ];
}

var NotEnoughSignatures = Caml_exceptions.create("TxWrapper.NotEnoughSignatures");

function finalize(usedInputs, param) {
  var tx = param[/* tx */0];
  try {
    Belt_Array.forEachWithIndexU(param[/* inputs */1], (function (idx, param) {
            var match = param[/* sequence */1] !== BitcoinjsLib.Transaction.DEFAULT_SEQUENCE;
            var nCoSigners = match ? 1 : Belt_Array.getExn(usedInputs, idx)[/* nCoSigners */4];
            var signatures = Belt_Array.slice(Belt_Array.keep(param[/* signatures */0], (function (prim) {
                        return BitcoinjsLib.script.isCanonicalScriptSignature(prim);
                      })), 0, nCoSigners);
            if (signatures.length < nCoSigners) {
              throw NotEnoughSignatures;
            }
            var witnessBuf = Js_option.getExn(getWitnessBuf(idx, tx));
            tx.setWitness(idx, Belt_Array.concatMany(/* array */[
                      /* array */[Buffer.alloc(0)],
                      signatures,
                      /* array */[witnessBuf]
                    ]));
            return /* () */0;
          }));
    return /* Ok */[tx];
  }
  catch (exn){
    if (exn === NotEnoughSignatures) {
      return /* NotEnoughSignatures */0;
    } else {
      throw exn;
    }
  }
}

var B = 0;

exports.B = B;
exports.extractInputs = extractInputs;
exports.make = make;
exports.needsSigning = needsSigning;
exports.pubKeyIndex = pubKeyIndex;
exports.sign = sign;
exports.getWitnessBuf = getWitnessBuf;
exports.merge = merge;
exports.NotEnoughSignatures = NotEnoughSignatures;
exports.finalize = finalize;
/* BTC Not a pure module */
