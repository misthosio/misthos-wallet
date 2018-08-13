// Generated by BUCKLESCRIPT VERSION 4.0.5, PLEASE EDIT WITH CARE
'use strict';

var BigNumber = require("../../ffi/BigNumber.bs.js");
var Caml_int64 = require("bs-platform/lib/js/caml_int64.js");
var Json_decode = require("@glennsl/bs-json/src/Json_decode.bs.js");
var BignumberJs = require("bignumber.js");

function fromSatoshis(satoshis) {
  return new BignumberJs.BigNumber(Caml_int64.to_float(satoshis));
}

function fromSatoshisFloat(satoshis) {
  return new BignumberJs.BigNumber(satoshis);
}

function toSatoshisFloat(btc) {
  return btc.integerValue(BignumberJs.ROUND_CEIL).toNumber();
}

var zero = new BignumberJs.BigNumber(0);

var satoshisPerBTC = new BignumberJs.BigNumber("1e8");

function fromString(btcString) {
  return satoshisPerBTC.times(new BignumberJs.BigNumber(btcString)).integerValue(BignumberJs.ROUND_FLOOR);
}

function format(btc) {
  return btc.dividedBy(satoshisPerBTC).toString();
}

function fromFloat(btcFloat) {
  return satoshisPerBTC.times(new BignumberJs.BigNumber(btcFloat));
}

function timesRounded(btc, n) {
  return btc.times(n).integerValue(BignumberJs.ROUND_CEIL);
}

function dividedByRounded(btc, n) {
  return btc.dividedBy(n).integerValue(BignumberJs.ROUND_FLOOR);
}

function encode(prim) {
  return prim.toJSON();
}

function decode(raw) {
  return new BignumberJs.BigNumber(Json_decode.string(raw));
}

var RoundingMode = BigNumber.RoundingMode;

exports.RoundingMode = RoundingMode;
exports.fromSatoshis = fromSatoshis;
exports.fromSatoshisFloat = fromSatoshisFloat;
exports.toSatoshisFloat = toSatoshisFloat;
exports.zero = zero;
exports.satoshisPerBTC = satoshisPerBTC;
exports.fromString = fromString;
exports.format = format;
exports.fromFloat = fromFloat;
exports.timesRounded = timesRounded;
exports.dividedByRounded = dividedByRounded;
exports.encode = encode;
exports.decode = decode;
/* zero Not a pure module */
