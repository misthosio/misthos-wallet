// Generated by BUCKLESCRIPT VERSION 3.0.0, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Utils = require("../../utils/Utils.bs.js");
var Js_option = require("bs-platform/lib/js/js_option.js");
var Json_decode = require("bs-json/src/Json_decode.js");
var Json_encode = require("bs-json/src/Json_encode.js");
var WalletTypes = require("./WalletTypes.bs.js");
var BitcoinjsLib = require("bitcoinjs-lib");
var Caml_primitive = require("bs-platform/lib/js/caml_primitive.js");
var PrimitiveTypes = require("../PrimitiveTypes.bs.js");
var AccountKeyChain = require("./AccountKeyChain.bs.js");
var CustodianKeyChain = require("./CustodianKeyChain.bs.js");

function next(coSigner, usedCoordinates, chainIdx, param) {
  var identifier = param[/* identifier */1];
  var accountIdx = param[/* accountIdx */0];
  var coSignerIdx = WalletTypes.CoSignerIndex[/* fromInt */1](Js_option.getExn(List.find(Js_option.isSome, List.mapi((function (i, param) {
                      var match = PrimitiveTypes.UserId[/* eq */5](param[0], coSigner);
                      if (match) {
                        return /* Some */[i];
                      } else {
                        return /* None */0;
                      }
                    }), List.sort((function (param, param$1) {
                          return Caml_primitive.caml_string_compare(Utils.bufToHex(param[1].getPublicKeyBuffer()), Utils.bufToHex(param$1[1].getPublicKeyBuffer()));
                        }), List.map((function (chain) {
                              return /* tuple */[
                                      chain[0],
                                      CustodianKeyChain.hdNode(chain[1])
                                    ];
                            }), param[/* custodianKeyChains */3]))))));
  var addressIdx = List.fold_left((function (res, param) {
          var addressIdx = param[4];
          if (WalletTypes.AccountIndex[/* eq */7](accountIdx, param[0]) && AccountKeyChain.Identifier[/* eq */4](identifier, param[1]) && WalletTypes.CoSignerIndex[/* eq */7](coSignerIdx, param[2]) && WalletTypes.ChainIndex[/* eq */7](chainIdx, param[3])) {
            var match = WalletTypes.AddressIndex[/* compare */6](addressIdx, res) > 0;
            if (match) {
              return addressIdx;
            } else {
              return res;
            }
          } else {
            return res;
          }
        }), WalletTypes.AddressIndex[/* fromInt */1](-1), usedCoordinates);
  return /* tuple */[
          accountIdx,
          identifier,
          coSignerIdx,
          chainIdx,
          WalletTypes.AddressIndex[/* next */3](addressIdx)
        ];
}

function nextInternal(user, usedCoordinates, accountKeyChain) {
  return next(user, usedCoordinates, WalletTypes.ChainIndex[/* internalChain */10], accountKeyChain);
}

function nextExternal(user, usedCoordinates, accountKeyChain) {
  return next(user, usedCoordinates, WalletTypes.ChainIndex[/* externalChain */9], accountKeyChain);
}

function accountIdx(param) {
  return param[0];
}

function keyChainIdent(param) {
  return param[1];
}

function coSignerIdx(param) {
  return param[2];
}

function chainIdx(param) {
  return param[3];
}

function addressIdx(param) {
  return param[4];
}

function allForAccount(aIdx) {
  return List.filter((function (c) {
                return WalletTypes.AccountIndex[/* eq */7](accountIdx(c), aIdx);
              }));
}

function encode(param) {
  var partial_arg = AccountKeyChain.Identifier[/* encode */0];
  var partial_arg$1 = WalletTypes.AccountIndex[/* encode */4];
  var partial_arg$2 = WalletTypes.AddressIndex[/* encode */4];
  var partial_arg$3 = WalletTypes.ChainIndex[/* encode */4];
  var partial_arg$4 = WalletTypes.CoSignerIndex[/* encode */4];
  return Json_encode.tuple2((function (param) {
                return Json_encode.tuple2(partial_arg$1, partial_arg, param);
              }), (function (param) {
                return Json_encode.tuple3(partial_arg$4, partial_arg$3, partial_arg$2, param);
              }), /* tuple */[
              /* tuple */[
                param[0],
                param[1]
              ],
              /* tuple */[
                param[2],
                param[3],
                param[4]
              ]
            ]);
}

function decode(raw) {
  var partial_arg = AccountKeyChain.Identifier[/* decode */1];
  var partial_arg$1 = WalletTypes.AccountIndex[/* decode */5];
  var partial_arg$2 = WalletTypes.AddressIndex[/* decode */5];
  var partial_arg$3 = WalletTypes.ChainIndex[/* decode */5];
  var partial_arg$4 = WalletTypes.CoSignerIndex[/* decode */5];
  var match = Json_decode.tuple2((function (param) {
          return Json_decode.tuple2(partial_arg$1, partial_arg, param);
        }), (function (param) {
          return Json_decode.tuple3(partial_arg$4, partial_arg$3, partial_arg$2, param);
        }), raw);
  var match$1 = match[1];
  var match$2 = match[0];
  return /* tuple */[
          match$2[0],
          match$2[1],
          match$1[0],
          match$1[1],
          match$1[2]
        ];
}

var Coordinates = /* module */[
  /* next */next,
  /* nextInternal */nextInternal,
  /* nextExternal */nextExternal,
  /* accountIdx */accountIdx,
  /* keyChainIdent */keyChainIdent,
  /* coSignerIdx */coSignerIdx,
  /* chainIdx */chainIdx,
  /* addressIdx */addressIdx,
  /* allForAccount */allForAccount,
  /* encode */encode,
  /* decode */decode
];

function encode$1(address) {
  return Json_encode.object_(/* :: */[
              /* tuple */[
                "nCoSigners",
                address[/* nCoSigners */0]
              ],
              /* :: */[
                /* tuple */[
                  "nPubKeys",
                  address[/* nPubKeys */1]
                ],
                /* :: */[
                  /* tuple */[
                    "coordinates",
                    encode(address[/* coordinates */2])
                  ],
                  /* :: */[
                    /* tuple */[
                      "witnessScript",
                      address[/* witnessScript */3]
                    ],
                    /* :: */[
                      /* tuple */[
                        "redeemScript",
                        address[/* redeemScript */4]
                      ],
                      /* :: */[
                        /* tuple */[
                          "displayAddress",
                          address[/* displayAddress */5]
                        ],
                        /* [] */0
                      ]
                    ]
                  ]
                ]
              ]
            ]);
}

function decode$1(raw) {
  return /* record */[
          /* nCoSigners */Json_decode.field("nCoSigners", Json_decode.$$int, raw),
          /* nPubKeys */Json_decode.field("nPubKeys", Json_decode.$$int, raw),
          /* coordinates */Json_decode.field("coordinates", decode, raw),
          /* witnessScript */Json_decode.field("witnessScript", Json_decode.string, raw),
          /* redeemScript */Json_decode.field("redeemScript", Json_decode.string, raw),
          /* displayAddress */Json_decode.field("displayAddress", Json_decode.string, raw)
        ];
}

function make(coordinates, param) {
  var custodianKeyChains = param[/* custodianKeyChains */3];
  var nCoSigners = param[/* nCoSigners */2];
  var keys = List.sort((function (pairA, pairB) {
          return Caml_primitive.caml_string_compare(Utils.bufToHex(pairA.getPublicKeyBuffer()), Utils.bufToHex(pairB.getPublicKeyBuffer()));
        }), List.map((function (node) {
              return node.keyPair;
            }), List.map((function (node) {
                  return node.derive(WalletTypes.CoSignerIndex[/* toInt */0](coSignerIdx(coordinates))).derive(WalletTypes.ChainIndex[/* toInt */0](chainIdx(coordinates))).derive(WalletTypes.AddressIndex[/* toInt */0](addressIdx(coordinates)));
                }), List.map((function (chain) {
                      return CustodianKeyChain.hdNode(chain[1]);
                    }), custodianKeyChains))));
  var witnessScript = BitcoinjsLib.script.multisig.output.encode(nCoSigners, $$Array.of_list(List.map((function (prim) {
                  return prim.getPublicKeyBuffer();
                }), keys)));
  var redeemScript = BitcoinjsLib.script.witnessScriptHash.output.encode(BitcoinjsLib.crypto.sha256(witnessScript));
  var outputScript = BitcoinjsLib.script.scriptHash.output.encode(BitcoinjsLib.crypto.hash160(redeemScript));
  var displayAddress = BitcoinjsLib.address.fromOutputScript(outputScript, List.hd(keys).getNetwork());
  return /* record */[
          /* nCoSigners */nCoSigners,
          /* nPubKeys */List.length(custodianKeyChains),
          /* coordinates */coordinates,
          /* witnessScript */Utils.bufToHex(witnessScript),
          /* redeemScript */Utils.bufToHex(redeemScript),
          /* displayAddress */displayAddress
        ];
}

function find(coordinates, keyChains) {
  return make(coordinates, AccountKeyChain.Collection[/* lookup */2](accountIdx(coordinates), keyChainIdent(coordinates), keyChains));
}

exports.Coordinates = Coordinates;
exports.encode = encode$1;
exports.decode = decode$1;
exports.make = make;
exports.find = find;
/* Utils Not a pure module */
