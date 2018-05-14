open PrimitiveTypes;

open Bitcoin;

let userSession = (userId, keyPair) : Session.Data.t => {
  let appPubKey = keyPair |> Utils.publicKeyFromKeyPair;
  let chainCode = appPubKey |. String.sub(0, 64) |> Utils.bufFromHex;
  {
    userId,
    appPrivateKey: keyPair |> ECPair.toWIF,
    issuerKeyPair: keyPair,
    storagePrefix: UserInfo.storagePrefix(~appPubKey),
    masterKeyChain: HDNode.make(keyPair, chainCode),
    network: Regtest,
  };
};

let threeUserSessions = {
  let (keyA, keyB, keyC) = (
    ECPair.fromWIFWithNetwork(
      "cUVTgxrs44T7zVon5dSDicBkBRjyfLwL7RF1RvR7n94ar3HEaLs1",
      Network.bitcoinNetwork(Regtest),
    ),
    ECPair.fromWIFWithNetwork(
      "cPMRPo3fXGehCmFC5QsSFcZmYivsFtLVexxWi22CFwocvndXLqP1",
      Network.bitcoinNetwork(Regtest),
    ),
    ECPair.fromWIFWithNetwork(
      "cPfdeLvhwvAVRRM5wiEWopWviGG65gbxQCHdtFL56PYUJXsTYixf",
      Network.bitcoinNetwork(Regtest),
    ),
  );
  (
    userSession("user1" |> UserId.fromString, keyA),
    userSession("user2" |> UserId.fromString, keyB),
    userSession("user3" |> UserId.fromString, keyC),
  );
};

let createVenture = user =>
  Generators.Log.make(
    user,
    {
      ...Generators.Event.createVenture(user),
      ventureId: VentureId.fromString("fixedVentureId"),
    },
  );

let encodeSessionData = (data: Session.Data.t) =>
  Json.Encode.(
    object_([
      ("userId", UserId.encode(data.userId)),
      ("issuerKeyPair", string(Bitcoin.ECPair.toWIF(data.issuerKeyPair))),
    ])
  );

let decodeSessionData = raw => {
  let userId = Json.Decode.(raw |> field("userId", UserId.decode));
  let issuerKeyPair =
    Json.Decode.(
      raw
      |> field("issuerKeyPair", string)
      |. Bitcoin.ECPair.fromWIFWithNetwork(Network.bitcoinNetwork(Regtest))
    );
  userSession(userId, issuerKeyPair);
};

let encodeFixture = (sessions, log) =>
  Json.Encode.(
    object_([
      ("sessions", array(encodeSessionData, sessions)),
      ("log", EventLog.encode(log)),
    ])
  );

let decodeFixture = raw => {
  let sessions =
    raw |> Json.Decode.(field("sessions", array(decodeSessionData)));
  let log = raw |> Json.Decode.field("log", EventLog.decode);
  (sessions, log);
};

let loadFixture = fileName =>
  try (
    Some(
      Node.Fs.readFileAsUtf8Sync(fileName ++ "-fixture.json")
      |> Json.parseOrRaise
      |> decodeFixture,
    )
  ) {
  | err =>
    Js.log2("couldn't load", err);
    None;
  };

let writeFixture = (fileName, sessions, log) =>
  try (
    Node.Fs.writeFileAsUtf8Sync(
      fileName ++ "-fixture.json",
      encodeFixture(sessions, log) |> Json.stringify,
    )
  ) {
  | _ => ()
  };

let basePath = "__tests__/fixtures/";

let withCached = (~scope, description, sessionsGenerator, generator, testBody) => {
  let replacedName = description |> Js.String.replaceByRe([%re "/ /g"], "_");
  let cacheFileName = basePath ++ scope ++ "-" ++ replacedName;
  let (sessions, log, cached) =
    switch (loadFixture(cacheFileName)) {
    | Some((sessions, eventLog)) => (
        sessions,
        Generators.Log.fromEventLog(eventLog),
        true,
      )
    | None =>
      let sessions = sessionsGenerator();
      (sessions, generator(sessions), false);
    };
  if (cached == false) {
    writeFixture(cacheFileName, sessions, log |> Generators.Log.eventLog);
  };
  Jest.describe(description, () =>
    testBody(sessions, log)
  );
};
