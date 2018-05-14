open Jest;

open PrimitiveTypes;

open WalletTypes;

open Event;

open ValidationHelpers;

let () =
  describe(" AccountKeyChainIdentified", () => {
    F.withCached(
      ~scope="AccountKeyChainIdentified",
      "when everything is fine",
      () => G.withUserSessions(2),
      sessions => {
        let (user1, _user2) = G.twoUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
          |> withCustodian(user1, ~supporters=[user1])
          |> withCustodianKeyChain(user1)
        );
      },
      (_sessions, log) =>
        testValidationResult(
          log |> constructState,
          L.(log |> withAccountKeyChainIdentified |> lastItem),
          Validation.Ok,
        ),
    );
    F.withCached(
      ~scope="AccountKeyChainIdentified",
      "when the account doesn't exist",
      () => G.withUserSessions(2),
      sessions => {
        let (user1, _user2) = G.twoUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
          |> withCustodian(user1, ~supporters=[user1])
          |> withCustodianKeyChain(user1)
        );
      },
      (_sessions, log) => {
        let identified: AccountKeyChainIdentified.t =
          L.(log |> withAccountKeyChainIdentified |> lastEvent)
          |> Event.getAccountKeyChainIdentifiedExn;
        testDataValidation(
          Validation.validateAccountKeyChainIdentified |> withSystemIssuer,
          log |> constructState,
          AccountKeyChainIdentified.{
            keyChain: {
              ...identified.keyChain,
              accountIdx: 1 |> AccountIndex.fromInt,
            },
          },
          Validation.BadData("Account doesn't exist"),
        );
      },
    );
    F.withCached(
      ~scope="AccountKeyChainIdentified",
      "when the AccountKeyChain is inconsistent",
      () => G.withUserSessions(2),
      sessions => {
        let (user1, _user2) = G.twoUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
          |> withCustodian(user1, ~supporters=[user1])
          |> withCustodianKeyChain(user1)
        );
      },
      (_sessions, log) => {
        let identified: AccountKeyChainIdentified.t =
          L.(log |> withAccountKeyChainIdentified |> lastEvent)
          |> Event.getAccountKeyChainIdentifiedExn;
        testDataValidation(
          Validation.validateAccountKeyChainIdentified |> withSystemIssuer,
          log |> constructState,
          AccountKeyChainIdentified.{
            keyChain: {
              ...identified.keyChain,
              identifier: "",
            },
          },
          Validation.BadData("Inconsistent AccountKeyChain"),
        );
      },
    );
    F.withCached(
      ~scope="AccountKeyChainIdentified",
      "with an old custodian",
      () => G.withUserSessions(2),
      sessions => {
        let (user1, user2) = G.twoUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
          |> withCustodian(user1, ~supporters=[user1])
          |> withCustodianKeyChain(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withCustodian(user2, ~supporters=[user1, user2])
          |> withCustodianKeyChain(user2)
          |> withAccountKeyChainIdentified
          |> withCustodianRemoved(user2, ~supporters=[user1])
          |> withPartnerRemoved(user2, ~supporters=[user1])
          |> withCustodianKeyChain(~keyChainIdx=1, user1)
        );
      },
      (sessions, log) => {
        let (user1, user2) = G.twoUserSessionsFromArray(sessions);
        testDataValidation(
          Validation.validateAccountKeyChainIdentified |> withSystemIssuer,
          log |> constructState,
          AccountKeyChainIdentified.{
            keyChain:
              AccountKeyChain.make(
                AccountIndex.default,
                [
                  (
                    user1.userId,
                    G.custodianKeyChain(
                      ~ventureId=log |> L.ventureId,
                      ~keyChainIdx=1,
                      user1,
                    ),
                  ),
                  (
                    user2.userId,
                    G.custodianKeyChain(
                      ~ventureId=log |> L.ventureId,
                      ~keyChainIdx=0,
                      user2,
                    ),
                  ),
                ],
              ),
          },
          Validation.BadData("Custodians aren't current"),
        );
      },
    );
    F.withCached(
      ~scope="AccountKeyChainIdentified",
      "when a CustodianKeyChain is unknown",
      () => G.withUserSessions(2),
      sessions => {
        let (user1, _user2) = G.twoUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
          |> withCustodian(user1, ~supporters=[user1])
          |> withCustodianKeyChain(user1)
        );
      },
      (sessions, log) => {
        let (user1, _user2) = G.twoUserSessionsFromArray(sessions);
        testDataValidation(
          Validation.validateAccountKeyChainIdentified |> withSystemIssuer,
          log |> constructState,
          AccountKeyChainIdentified.{
            keyChain:
              AccountKeyChain.make(
                AccountIndex.default,
                [
                  (
                    user1.userId,
                    G.custodianKeyChain(
                      ~ventureId=VentureId.make(),
                      ~keyChainIdx=1,
                      user1,
                    ),
                  ),
                ],
              ),
          },
          Validation.BadData("Bad CustodianKeyChain"),
        );
      },
    );
  });

describe("AccountKeyChainActivated", () => {
  F.withCached(
    ~scope="AccountKeyChainActivated",
    "when everything is fine",
    () => G.withUserSessions(2),
    sessions => {
      let (user1, _user2) = G.twoUserSessionsFromArray(sessions);
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withAccount(~supporter=user1)
        |> withCustodian(user1, ~supporters=[user1])
        |> withCustodianKeyChain(user1)
        |> withAccountKeyChainIdentified
      );
    },
    (sessions, log) => {
      let (user1, _user2) = G.twoUserSessionsFromArray(sessions);
      testValidationResult(
        log |> constructState,
        L.(log |> withAccountKeyChainActivated(user1) |> lastItem),
        Validation.Ok,
      );
    },
  );
  F.withCached(
    ~scope="AccountKeyChainActivated",
    "after a partner removal",
    () => G.withUserSessions(2),
    sessions => {
      let (user1, user2) = G.twoUserSessionsFromArray(sessions);
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withAccount(~supporter=user1)
        |> withCustodian(user1, ~supporters=[user1])
        |> withCustodianKeyChain(user1)
        |> withAccountKeyChainIdentified
        |> withAccountKeyChainActivated(user1)
        |> withPartner(user2, ~supporters=[user1])
        |> withCustodian(user2, ~supporters=[user1, user2])
        |> withCustodianKeyChain(user2)
        |> withAccountKeyChainIdentified
        |> withAccountKeyChainActivated(user2)
        |> withAccountKeyChainActivated(user1)
        |> withPartnerRemoved(user2, ~supporters=[user1])
      );
    },
    (sessions, log) => {
      let (user1, _user2) = G.twoUserSessionsFromArray(sessions);
      testValidationResult(
        log |> constructState,
        L.(
          log |> withAccountKeyChainActivated(~sequence=1, user1) |> lastItem
        ),
        Validation.Ok,
      );
    },
  );
  F.withCached(
    ~scope="AccountKeyChainActivated",
    "when the account doesn't exist",
    () => G.withUserSessions(2),
    sessions => {
      let (user1, _user2) = G.twoUserSessionsFromArray(sessions);
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withAccount(~supporter=user1)
        |> withCustodian(user1, ~supporters=[user1])
        |> withCustodianKeyChain(user1)
        |> withAccountKeyChainIdentified
      );
    },
    (sessions, log) => {
      let (user1, _user2) = G.twoUserSessionsFromArray(sessions);
      let activated: AccountKeyChainActivated.t =
        L.(log |> withAccountKeyChainActivated(user1) |> lastEvent)
        |> Event.getAccountKeyChainActivatedExn;
      testDataValidation(
        Validation.validateAccountKeyChainActivated |> withIssuer(user1),
        log |> constructState,
        AccountKeyChainActivated.{
          ...activated,
          accountIdx: 1 |> AccountIndex.fromInt,
        },
        Validation.BadData("Account doesn't exist"),
      );
    },
  );
  F.withCached(
    ~scope="AccountKeyChainActivated",
    "when the issuer doesn't match",
    () => G.withUserSessions(2),
    sessions => {
      let (user1, user2) = G.twoUserSessionsFromArray(sessions);
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withAccount(~supporter=user1)
        |> withCustodian(user1, ~supporters=[user1])
        |> withCustodianKeyChain(user1)
        |> withAccountKeyChainIdentified
        |> withPartner(user2, ~supporters=[user1])
      );
    },
    (sessions, log) => {
      let (user1, user2) = G.twoUserSessionsFromArray(sessions);
      let activated: AccountKeyChainActivated.t =
        L.(log |> withAccountKeyChainActivated(user1) |> lastEvent)
        |> Event.getAccountKeyChainActivatedExn;
      testDataValidation(
        Validation.validateAccountKeyChainActivated |> withIssuer(user2),
        log |> constructState,
        activated,
        InvalidIssuer,
      );
    },
  );
  F.withCached(
    ~scope="AccountKeyChainActivated",
    "when the issuer is not a custodian",
    () => G.withUserSessions(2),
    sessions => {
      let (user1, user2) = G.twoUserSessionsFromArray(sessions);
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withAccount(~supporter=user1)
        |> withCustodian(user1, ~supporters=[user1])
        |> withCustodianKeyChain(user1)
        |> withAccountKeyChainIdentified
        |> withPartner(user2, ~supporters=[user1])
      );
    },
    (sessions, log) => {
      let (_user1, user2) = G.twoUserSessionsFromArray(sessions);
      let activated: AccountKeyChainActivated.t =
        L.(log |> withAccountKeyChainActivated(user2) |> lastEvent)
        |> Event.getAccountKeyChainActivatedExn;
      testDataValidation(
        Validation.validateAccountKeyChainActivated |> withIssuer(user2),
        log |> constructState,
        activated,
        BadData("Not a custodian"),
      );
    },
  );
  F.withCached(
    ~scope="AccountKeyChainActivated",
    "when the identifier is unknown",
    () => G.withUserSessions(2),
    sessions => {
      let (user1, _user2) = G.twoUserSessionsFromArray(sessions);
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withAccount(~supporter=user1)
        |> withCustodian(user1, ~supporters=[user1])
        |> withCustodianKeyChain(user1)
        |> withAccountKeyChainIdentified
      );
    },
    (sessions, log) => {
      let (user1, _user2) = G.twoUserSessionsFromArray(sessions);
      let activated: AccountKeyChainActivated.t =
        L.(log |> withAccountKeyChainActivated(user1) |> lastEvent)
        |> Event.getAccountKeyChainActivatedExn;
      testDataValidation(
        Validation.validateAccountKeyChainActivated |> withIssuer(user1),
        log |> constructState,
        {...activated, identifier: "bad"},
        BadData("Unknown AccountKeyChain identifier"),
      );
    },
  );
  F.withCached(
    ~scope="AccountKeyChainActivated",
    "when the sequence is not in order",
    () => G.withUserSessions(2),
    sessions => {
      let (user1, _user2) = G.twoUserSessionsFromArray(sessions);
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withAccount(~supporter=user1)
        |> withCustodian(user1, ~supporters=[user1])
        |> withCustodianKeyChain(user1)
        |> withAccountKeyChainIdentified
      );
    },
    (sessions, log) => {
      let (user1, _user2) = G.twoUserSessionsFromArray(sessions);
      let activated: AccountKeyChainActivated.t =
        L.(log |> withAccountKeyChainActivated(user1) |> lastEvent)
        |> Event.getAccountKeyChainActivatedExn;
      testDataValidation(
        Validation.validateAccountKeyChainActivated |> withIssuer(user1),
        log |> constructState,
        {...activated, sequence: 1},
        BadData("AccountKeyChain sequence out of order"),
      );
    },
  );
});
