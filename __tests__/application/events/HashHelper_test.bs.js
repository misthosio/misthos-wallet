// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE
'use strict';

var Jest = require("@glennsl/bs-jest/src/jest.js");
var Json = require("@glennsl/bs-json/src/Json.bs.js");
var HashHelper = require("../../../src/application/events/HashHelper.bs.js");
var Json_encode = require("@glennsl/bs-json/src/Json_encode.bs.js");

function encode(a, b) {
  return Json_encode.object_(/* :: */[
              /* tuple */[
                "a",
                Json_encode.nullable((function (prim) {
                        return prim;
                      }), a)
              ],
              /* :: */[
                /* tuple */[
                  "b",
                  Json_encode.array((function (param) {
                          return Json_encode.nullable((function (prim) {
                                        return prim;
                                      }), param);
                        }), /* array */[b])
                ],
                /* [] */0
              ]
            ]);
}

describe("HashHelper.pruneNullFields", (function () {
        Jest.test("Prunes null values", (function () {
                return Jest.Expect[/* toEqual */12]("{\"b\":[\"bla\"]}", Jest.Expect[/* expect */0](Json.stringify(HashHelper.pruneNullFields(encode(/* None */0, /* Some */["bla"])))));
              }));
        return Jest.test("Does not change arrays", (function () {
                      return Jest.Expect[/* toEqual */12]("{\"a\":1,\"b\":[null]}", Jest.Expect[/* expect */0](Json.stringify(HashHelper.pruneNullFields(encode(/* Some */[1], /* None */0)))));
                    }));
      }));

exports.encode = encode;
/*  Not a pure module */
