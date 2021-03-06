// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var Belt_Id = require("bs-platform/lib/js/belt_Id.js");
var Belt_Map = require("bs-platform/lib/js/belt_Map.js");
var Belt_Set = require("bs-platform/lib/js/belt_Set.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Json_decode = require("@glennsl/bs-json/src/Json_decode.bs.js");
var Caml_primitive = require("bs-platform/lib/js/caml_primitive.js");

function compareUtxos(param, param$1) {
  var c = Caml_primitive.caml_string_compare(param[/* txId */0], param$1[/* txId */0]);
  if (c !== 0) {
    return c;
  } else {
    return Caml_primitive.caml_int_compare(param[/* txOutputN */1], param$1[/* txOutputN */1]);
  }
}

var include = Belt_Id.MakeComparableU(/* module */[/* cmp */compareUtxos]);

var cmp = include[0];

var emptyUtxoSet = Belt_Set.make([cmp]);

function next(idx) {
  return idx + 1 | 0;
}

function encode(id) {
  return id;
}

var decode = Json_decode.$$int;

var compare = Caml_obj.caml_compare;

var eq = Caml_obj.caml_equal;

function neq(a, b) {
  return Caml_obj.caml_compare(a, b) !== 0;
}

var cmp$1 = Caml_obj.caml_compare;

var Comparator = Belt_Id.MakeComparableU(/* module */[/* cmp */cmp$1]);

function makeMap(param) {
  return Belt_Map.make(Comparator);
}

var UtxoCmp = [cmp];

function AccountIndex_000(prim) {
  return prim;
}

function AccountIndex_001(prim) {
  return prim;
}

var AccountIndex = [
  AccountIndex_000,
  AccountIndex_001,
  0,
  next,
  encode,
  decode,
  compare,
  eq,
  neq,
  Comparator,
  makeMap,
  0
];

function CustodianKeyChainIndex_000(prim) {
  return prim;
}

function CustodianKeyChainIndex_001(prim) {
  return prim;
}

var CustodianKeyChainIndex = [
  CustodianKeyChainIndex_000,
  CustodianKeyChainIndex_001,
  next,
  encode,
  decode,
  compare,
  eq,
  neq,
  Comparator,
  makeMap,
  0
];

function AccountKeyChainIndex_000(prim) {
  return prim;
}

function AccountKeyChainIndex_001(prim) {
  return prim;
}

var AccountKeyChainIndex = [
  AccountKeyChainIndex_000,
  AccountKeyChainIndex_001,
  0,
  next,
  encode,
  decode,
  compare,
  eq,
  neq,
  Comparator,
  makeMap
];

function CoSignerIndex_000(prim) {
  return prim;
}

function CoSignerIndex_001(prim) {
  return prim;
}

var CoSignerIndex = [
  CoSignerIndex_000,
  CoSignerIndex_001,
  0,
  next,
  encode,
  decode,
  compare,
  eq,
  neq,
  Comparator,
  makeMap
];

function ChainIndex_000(prim) {
  return prim;
}

function ChainIndex_001(prim) {
  return prim;
}

var ChainIndex = [
  ChainIndex_000,
  ChainIndex_001,
  0,
  next,
  encode,
  decode,
  compare,
  eq,
  neq,
  Comparator,
  makeMap,
  0,
  1
];

function AddressIndex_000(prim) {
  return prim;
}

function AddressIndex_001(prim) {
  return prim;
}

var AddressIndex = [
  AddressIndex_000,
  AddressIndex_001,
  0,
  next,
  encode,
  decode,
  compare,
  eq,
  neq,
  Comparator,
  makeMap
];

exports.UtxoCmp = UtxoCmp;
exports.emptyUtxoSet = emptyUtxoSet;
exports.AccountIndex = AccountIndex;
exports.CustodianKeyChainIndex = CustodianKeyChainIndex;
exports.AccountKeyChainIndex = AccountKeyChainIndex;
exports.CoSignerIndex = CoSignerIndex;
exports.ChainIndex = ChainIndex;
exports.AddressIndex = AddressIndex;
/* include Not a pure module */
