// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE
'use strict';

var Jest = require("@glennsl/bs-jest/src/jest.js");
var Utils = require("../../../src/utils/Utils.bs.js");

describe("hashCode", (function () {
        Jest.test("hashes an empty string", (function () {
                return Jest.Expect[/* toBe */2](0, Jest.Expect[/* expect */0](Utils.hashCode("")));
              }));
        return Jest.test("hashes a string", (function () {
                      return Jest.Expect[/* toBe */2](97692050, Jest.Expect[/* expect */0](Utils.hashCode("frank")));
                    }));
      }));

/*  Not a pure module */
