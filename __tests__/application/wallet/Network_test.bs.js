// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE
'use strict';

var Jest = require("@glennsl/bs-jest/src/jest.js");
var Network = require("../../../src/application/wallet/Network.bs.js");
var BitcoinjsLib = require("bitcoinjs-lib");

describe("Income addresses", (function () {
        Jest.test("testnetIncomeAddress can be used", (function () {
                return Jest.Expect[/* toEqual */12](Network.incomeAddress(/* Testnet */1), Jest.Expect[/* expect */0](BitcoinjsLib.address.fromOutputScript(BitcoinjsLib.address.toOutputScript(Network.incomeAddress(/* Testnet */1), Network.bitcoinNetwork(/* Testnet */1)), Network.bitcoinNetwork(/* Testnet */1))));
              }));
        return Jest.test("regtestIncomeAddress can be used", (function () {
                      return Jest.Expect[/* toEqual */12](Network.incomeAddress(/* Regtest */0), Jest.Expect[/* expect */0](BitcoinjsLib.address.fromOutputScript(BitcoinjsLib.address.toOutputScript(Network.incomeAddress(/* Regtest */0), Network.bitcoinNetwork(/* Regtest */0)), Network.bitcoinNetwork(/* Regtest */0))));
                    }));
      }));

/*  Not a pure module */
