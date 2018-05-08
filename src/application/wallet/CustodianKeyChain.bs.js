// Generated by BUCKLESCRIPT VERSION 3.0.0, PLEASE EDIT WITH CARE
'use strict';

var Utils = require("../../utils/Utils.bs.js");
var Bitcoin = require("../../ffi/Bitcoin.bs.js");
var Json_decode = require("bs-json/src/Json_decode.js");
var Json_encode = require("bs-json/src/Json_encode.js");
var WalletTypes = require("./WalletTypes.bs.js");
var PrimitiveTypes = require("../PrimitiveTypes.bs.js");

function accountIdx($$public) {
  return $$public[/* accountIdx */0];
}

function keyChainIdx($$public) {
  return $$public[/* keyChainIdx */1];
}

function hdNode($$public) {
  return $$public[/* hdNode */2];
}

function make(ventureId, accountIdx, keyChainIdx, masterKeyChain) {
  var misthosKeyChain = masterKeyChain.deriveHardened(0);
  var salt = Utils.hash(Utils.bufToHex(misthosKeyChain.getPublicKeyBuffer()));
  var custodianKeyChain = misthosKeyChain.deriveHardened(Utils.hashCode(Utils.hash(PrimitiveTypes.VentureId[/* toString */0](ventureId) + salt))).deriveHardened(0).deriveHardened(WalletTypes.AccountIndex[/* toInt */0](accountIdx)).deriveHardened(WalletTypes.CustodianKeyChainIndex[/* toInt */0](keyChainIdx)).deriveHardened(45);
  return /* record */[
          /* accountIdx */accountIdx,
          /* keyChainIdx */keyChainIdx,
          /* hdNode */custodianKeyChain
        ];
}

function toPublicKeyChain(keyChain) {
  return /* record */[
          /* accountIdx */keyChain[/* accountIdx */0],
          /* keyChainIdx */keyChain[/* keyChainIdx */1],
          /* hdNode */keyChain[/* hdNode */2].neutered()
        ];
}

function getSigningKey(coSignerIdx, chainIdx, addressIdx, keyChain) {
  return keyChain[/* hdNode */2].derive(WalletTypes.CoSignerIndex[/* toInt */0](coSignerIdx)).derive(WalletTypes.ChainIndex[/* toInt */0](chainIdx)).derive(WalletTypes.AddressIndex[/* toInt */0](addressIdx)).keyPair;
}

function encode(keyChain) {
  return Json_encode.object_(/* :: */[
              /* tuple */[
                "accountIndex",
                WalletTypes.AccountIndex[/* encode */4](keyChain[/* accountIdx */0])
              ],
              /* :: */[
                /* tuple */[
                  "keyChainIndex",
                  WalletTypes.CustodianKeyChainIndex[/* encode */3](keyChain[/* keyChainIdx */1])
                ],
                /* :: */[
                  /* tuple */[
                    "hdNode",
                    keyChain[/* hdNode */2].toBase58()
                  ],
                  /* [] */0
                ]
              ]
            ]);
}

function decode(raw) {
  return /* record */[
          /* accountIdx */Json_decode.field("accountIndex", WalletTypes.AccountIndex[/* decode */5], raw),
          /* keyChainIdx */Json_decode.field("keyChainIndex", WalletTypes.CustodianKeyChainIndex[/* decode */4], raw),
          /* hdNode */Bitcoin.HDNode[/* fromBase58 */0](Json_decode.field("hdNode", Json_decode.string, raw))
        ];
}

exports.make = make;
exports.toPublicKeyChain = toPublicKeyChain;
exports.accountIdx = accountIdx;
exports.keyChainIdx = keyChainIdx;
exports.getSigningKey = getSigningKey;
exports.hdNode = hdNode;
exports.encode = encode;
exports.decode = decode;
/* Utils Not a pure module */
