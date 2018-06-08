// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Bigi = require("bigi");
var $$Array = require("bs-platform/lib/js/array.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Bitcoin = require("../ffi/Bitcoin.bs.js");
var Caml_string = require("bs-platform/lib/js/caml_string.js");
var Json_decode = require("bs-json/src/Json_decode.js");
var BitcoinjsLib = require("bitcoinjs-lib");

function bufToHex(param) {
  return param.toString("hex");
}

function bufFromHex(param) {
  return Buffer.from(param, "hex");
}

function hexByteLength(param) {
  return Buffer.byteLength(param, "hex");
}

function keyPairFromPrivateKey(network, key) {
  return Bitcoin.ECPair[/* makeWithNetwork */1](Bigi.fromHex(key), network);
}

function publicKeyFromKeyPair(pair) {
  return bufToHex(pair.getPublicKeyBuffer());
}

function keyFromPublicKey(key) {
  return BitcoinjsLib.ECPair.fromPublicKeyBuffer(bufFromHex(key));
}

function signatureToString(ecSignature) {
  return bufToHex(ecSignature.toDER());
}

function signatureFromString(ecSignature) {
  return BitcoinjsLib.ECSignature.fromDER(bufFromHex(ecSignature));
}

function hash(s) {
  return bufToHex(BitcoinjsLib.crypto.sha256(s));
}

function hashCode(s) {
  return List.fold_left((function (h, c) {
                var h$1 = ((h << 5) - h | 0) + Caml_string.get(c, 0) | 0;
                return h$1 & h$1;
              }), 0, $$Array.to_list(Array.from(s))) & 2147483647;
}

function $great$great(f, g, v) {
  return Curry._1(g, Curry._1(f, v));
}

function printError(message, error) {
  console.log("Error - " + (message + ":"));
  console.log(error);
  return /* () */0;
}

function mapOption(fn, param) {
  if (param) {
    return /* Some */[Curry._1(fn, param[0])];
  } else {
    return /* None */0;
  }
}

function andThen(fn, param) {
  if (param) {
    return Curry._1(fn, param[0]);
  } else {
    return /* None */0;
  }
}

function encodeFloat(prim) {
  return prim;
}

function intersperse(fn, items) {
  var param = List.rev(List.flatten(List.mapi((function (i, item) {
                  return /* :: */[
                          item,
                          /* :: */[
                            Curry._1(fn, String(i + List.length(items) | 0)),
                            /* [] */0
                          ]
                        ];
                }), items)));
  return List.rev(param ? param[1] : /* [] */0);
}

var decodeFloat = Json_decode.$$float;

exports.bufToHex = bufToHex;
exports.bufFromHex = bufFromHex;
exports.hexByteLength = hexByteLength;
exports.keyPairFromPrivateKey = keyPairFromPrivateKey;
exports.publicKeyFromKeyPair = publicKeyFromKeyPair;
exports.keyFromPublicKey = keyFromPublicKey;
exports.signatureToString = signatureToString;
exports.signatureFromString = signatureFromString;
exports.hash = hash;
exports.hashCode = hashCode;
exports.$great$great = $great$great;
exports.printError = printError;
exports.mapOption = mapOption;
exports.andThen = andThen;
exports.encodeFloat = encodeFloat;
exports.decodeFloat = decodeFloat;
exports.intersperse = intersperse;
/* bigi Not a pure module */
