// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var Jest = require("@glennsl/bs-jest/src/jest.js");
var Utils = require("../../src/utils/Utils.bs.js");
var $$String = require("bs-platform/lib/js/string.js");
var Bitcoin = require("../../src/ffi/Bitcoin.bs.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var BitcoinjsLib = require("bitcoinjs-lib");

var network = BitcoinjsLib.networks.testnet;

var pair = BitcoinjsLib.ECPair.fromWIF("92Qba5hnyWSn5Ffcka56yMQauaWY6ZLd91Vzxbi4a9CCetaHtYj", network);

describe("ECPair", (function () {
        Jest.test("can access public key", (function (param) {
                return Jest.Expect[/* toEqual */12]("044289801366bcee6172b771cf5a7f13aaecd237a0b9a1ff9d769cabc2e6b70a34cec320a0565fb7caf11b1ca2f445f9b7b012dda5718b3cface369ee3a034ded6", Jest.Expect[/* expect */0](Utils.publicKeyFromKeyPair(pair)));
              }));
        return Jest.test("can access network", (function (param) {
                      return Jest.Expect[/* toEqual */12](network, Jest.Expect[/* expect */0](pair.network));
                    }));
      }));

describe("Address", (function () {
        return Jest.test("can return an address from a key pair", (function (param) {
                      return Jest.Expect[/* toEqual */12]("mgWUuj1J1N882jmqFxtDepEC73Rr22E9GU", Jest.Expect[/* expect */0](Bitcoin.Address[/* fromKeyPair */2](pair)));
                    }));
      }));

describe("Payments", (function () {
        var keys = /* array */[
          BitcoinjsLib.ECPair.fromWIF("cUVTgxrs44T7zVon5dSDicBkBRjyfLwL7RF1RvR7n94ar3HEaLs1", network),
          BitcoinjsLib.ECPair.fromWIF("cPMRPo3fXGehCmFC5QsSFcZmYivsFtLVexxWi22CFwocvndXLqP1", network),
          BitcoinjsLib.ECPair.fromWIF("cPfdeLvhwvAVRRM5wiEWopWviGG65gbxQCHdtFL56PYUJXsTYixf", network)
        ];
        return Jest.test("multisig", (function (param) {
                      var ret = BitcoinjsLib.payments.p2sh({
                            redeem: BitcoinjsLib.payments.p2wsh({
                                  redeem: BitcoinjsLib.payments.p2ms({
                                        m: 2,
                                        pubkeys: Belt_Array.map(keys, (function (prim) {
                                                return prim.publicKey;
                                              })),
                                        network: network
                                      }),
                                  network: network
                                }),
                            network: network
                          });
                      return Jest.Expect[/* toEqual */12]("2Mw8JgQijki6NkAwGZiWa4XdnE4be1H93ku", Jest.Expect[/* expect */0](ret.address));
                    }));
      }));

describe("HDNode", (function () {
        var pubkey = Utils.publicKeyFromKeyPair(pair);
        var chainCode = Utils.bufFromHex($$String.sub(pubkey, 0, 64));
        return Jest.test("can create an HDNode", (function (param) {
                      var node = BitcoinjsLib.bip32.fromPrivateKey(pair.privateKey, chainCode, pair.network);
                      return Jest.Expect[/* toEqual */12]("024289801366bcee6172b771cf5a7f13aaecd237a0b9a1ff9d769cabc2e6b70a34", Jest.Expect[/* expect */0](Utils.bufToHex(node.publicKey)));
                    }));
      }));

/* network Not a pure module */
