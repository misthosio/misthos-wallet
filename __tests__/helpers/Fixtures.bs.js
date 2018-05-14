// Generated by BUCKLESCRIPT VERSION 3.0.0, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Json = require("bs-json/src/Json.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Utils = require("../../src/utils/Utils.bs.js");
var $$String = require("bs-platform/lib/js/string.js");
var Network = require("../../src/application/wallet/Network.bs.js");
var EventLog = require("../../src/application/events/EventLog.bs.js");
var UserInfo = require("../../src/application/UserInfo.bs.js");
var Generators = require("./Generators.bs.js");
var Json_decode = require("bs-json/src/Json_decode.js");
var Json_encode = require("bs-json/src/Json_encode.js");
var BitcoinjsLib = require("bitcoinjs-lib");
var PrimitiveTypes = require("../../src/application/PrimitiveTypes.bs.js");

function userSession(userId, keyPair) {
  var appPubKey = Utils.publicKeyFromKeyPair(keyPair);
  var chainCode = Utils.bufFromHex($$String.sub(appPubKey, 0, 64));
  return /* record */[
          /* userId */userId,
          /* appPrivateKey */keyPair.toWIF(),
          /* issuerKeyPair */keyPair,
          /* storagePrefix */UserInfo.storagePrefix(appPubKey),
          /* masterKeyChain */new BitcoinjsLib.HDNode(keyPair, chainCode),
          /* network : Regtest */0
        ];
}

var keyA = BitcoinjsLib.ECPair.fromWIF("cUVTgxrs44T7zVon5dSDicBkBRjyfLwL7RF1RvR7n94ar3HEaLs1", Network.bitcoinNetwork(/* Regtest */0));

var keyB = BitcoinjsLib.ECPair.fromWIF("cPMRPo3fXGehCmFC5QsSFcZmYivsFtLVexxWi22CFwocvndXLqP1", Network.bitcoinNetwork(/* Regtest */0));

var keyC = BitcoinjsLib.ECPair.fromWIF("cPfdeLvhwvAVRRM5wiEWopWviGG65gbxQCHdtFL56PYUJXsTYixf", Network.bitcoinNetwork(/* Regtest */0));

var threeUserSessions_000 = userSession(PrimitiveTypes.UserId[/* fromString */1]("user1"), keyA);

var threeUserSessions_001 = userSession(PrimitiveTypes.UserId[/* fromString */1]("user2"), keyB);

var threeUserSessions_002 = userSession(PrimitiveTypes.UserId[/* fromString */1]("user3"), keyC);

var threeUserSessions = /* tuple */[
  threeUserSessions_000,
  threeUserSessions_001,
  threeUserSessions_002
];

var threeUserSessionsArray = /* array */[
  threeUserSessions_000,
  threeUserSessions_001,
  threeUserSessions_002
];

function createVenture(user) {
  var init = Generators.Event[/* createVenture */0](user);
  return Generators.Log[/* make */9](user, /* record */[
              /* ventureId */PrimitiveTypes.VentureId[/* fromString */1]("fixedVentureId"),
              /* ventureName */init[/* ventureName */1],
              /* creatorId */init[/* creatorId */2],
              /* creatorPubKey */init[/* creatorPubKey */3],
              /* metaPolicy */init[/* metaPolicy */4],
              /* systemIssuer */init[/* systemIssuer */5],
              /* network */init[/* network */6]
            ]);
}

function encodeSessionData(data) {
  return Json_encode.object_(/* :: */[
              /* tuple */[
                "userId",
                PrimitiveTypes.UserId[/* encode */2](data[/* userId */0])
              ],
              /* :: */[
                /* tuple */[
                  "issuerKeyPair",
                  data[/* issuerKeyPair */2].toWIF()
                ],
                /* [] */0
              ]
            ]);
}

function decodeSessionData(raw) {
  var userId = Json_decode.field("userId", PrimitiveTypes.UserId[/* decode */3], raw);
  var issuerKeyPair = BitcoinjsLib.ECPair.fromWIF(Json_decode.field("issuerKeyPair", Json_decode.string, raw), Network.bitcoinNetwork(/* Regtest */0));
  return userSession(userId, issuerKeyPair);
}

function encodeFixture(sessions, log) {
  return Json_encode.object_(/* :: */[
              /* tuple */[
                "sessions",
                Json_encode.array(encodeSessionData, sessions)
              ],
              /* :: */[
                /* tuple */[
                  "log",
                  Curry._1(EventLog.encode, log)
                ],
                /* [] */0
              ]
            ]);
}

function decodeFixture(raw) {
  var sessions = Json_decode.field("sessions", (function (param) {
          return Json_decode.array(decodeSessionData, param);
        }), raw);
  var log = Json_decode.field("log", EventLog.decode, raw);
  return /* tuple */[
          sessions,
          log
        ];
}

function loadFixture(fileName) {
  try {
    return /* Some */[decodeFixture(Json.parseOrRaise(Fs.readFileSync(fileName + "-fixture.json", "utf8")))];
  }
  catch (exn){
    return /* None */0;
  }
}

function writeFixture(fileName, sessions, log) {
  try {
    Fs.writeFileSync(fileName + "-fixture.json", Json.stringify(encodeFixture(sessions, log)), "utf8");
    return /* () */0;
  }
  catch (exn){
    return /* () */0;
  }
}

var basePath = "__tests__/fixtures/";

function withCached($staropt$star, scope, description, sessionsGenerator, generator, testBody) {
  var load = $staropt$star ? $staropt$star[0] : true;
  var replacedName = description.replace((/ /g), "_");
  var cacheFileName = basePath + (scope + ("-" + replacedName));
  var match = loadFixture(cacheFileName);
  var match$1;
  var exit = 0;
  if (load && match) {
    var match$2 = match[0];
    match$1 = /* tuple */[
      match$2[0],
      Generators.Log[/* fromEventLog */8](match$2[1]),
      true
    ];
  } else {
    exit = 1;
  }
  if (exit === 1) {
    var sessions = Curry._1(sessionsGenerator, /* () */0);
    match$1 = /* tuple */[
      sessions,
      Curry._1(generator, sessions),
      false
    ];
  }
  var log = match$1[1];
  var sessions$1 = match$1[0];
  if (match$1[2] === false) {
    writeFixture(cacheFileName, sessions$1, Generators.Log[/* eventLog */5](log));
  }
  describe(description, (function () {
          return Curry._2(testBody, sessions$1, log);
        }));
  return /* () */0;
}

exports.userSession = userSession;
exports.threeUserSessions = threeUserSessions;
exports.threeUserSessionsArray = threeUserSessionsArray;
exports.createVenture = createVenture;
exports.encodeSessionData = encodeSessionData;
exports.decodeSessionData = decodeSessionData;
exports.encodeFixture = encodeFixture;
exports.decodeFixture = decodeFixture;
exports.loadFixture = loadFixture;
exports.writeFixture = writeFixture;
exports.basePath = basePath;
exports.withCached = withCached;
/* keyA Not a pure module */
