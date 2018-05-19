open Jest;

open PrimitiveTypes;

open WalletTypes;

open Event;

open ValidationHelpers;

let () =
  describe("CustodianProposed", () => {
    F.withCached(
      ~scope="CustodianProposed",
      "when proposing a custodian",
      () => G.withUserSessions(2),
      sessions => {
        let (user1, user2) = G.twoUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
          |> withCustodian(user1, ~supporters=[user1])
          |> withPartner(user2, ~supporters=[user1])
        );
      },
      (sessions, log) => {
        let (user1, user2) = G.twoUserSessionsFromArray(sessions);
        testValidationResult(
          log |> constructState,
          L.(
            log
            |> withCustodianProposed(~supporter=user1, ~custodian=user2)
            |> lastItem
          ),
          Validation.Ok,
        );
      },
    );
    F.withCached(
      ~scope="CustodianProposed",
      "when proposing a custodian after removal",
      () => G.withUserSessions(2),
      sessions => {
        let (user1, user2) = G.twoUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
          |> withCustodian(user1, ~supporters=[user1])
          |> withPartner(user2, ~supporters=[user1])
          |> withCustodian(user2, ~supporters=[user1, user2])
          |> withCustodianRemoved(user2, ~supporters=[user1, user2])
        );
      },
      (sessions, log) => {
        let (user1, user2) = G.twoUserSessionsFromArray(sessions);
        testValidationResult(
          log |> constructState,
          L.(
            log
            |> withCustodianProposed(~supporter=user1, ~custodian=user2)
            |> lastItem
          ),
          Validation.Ok,
        );
      },
    );
    describe("validateCustodianData", () => {
      F.withCached(
        ~scope="CustodianProposed",
        "when the custodian is not a partner",
        () => G.withUserSessions(3),
        sessions => {
          let (user1, user2, _user3) =
            G.threeUserSessionsFromArray(sessions);
          L.(
            createVenture(user1)
            |> withFirstPartner(user1)
            |> withAccount(~supporter=user1)
            |> withPartner(user2, ~supporters=[user1])
          );
        },
        (sessions, log) => {
          let (_user1, _user2, user3) =
            G.threeUserSessionsFromArray(sessions);
          let partnerApproval =
            log |> L.lastEvent |> Event.getPartnerAcceptedExn;
          testDataValidation(
            Validation.validateCustodianData,
            log |> constructState,
            Custodian.Data.{
              lastCustodianRemovalProcess: None,
              partnerId: user3.userId,
              partnerApprovalProcess: partnerApproval.processId,
              accountIdx: AccountIndex.default,
            },
            Validation.BadData(
              "Partner approval process doesn't match user id",
            ),
          );
        },
      );
      F.withCached(
        ~scope="CustodianProposed",
        "when the partner approval process reference is wrong",
        () => G.withUserSessions(2),
        sessions => {
          let (user1, user2) = G.twoUserSessionsFromArray(sessions);
          L.(
            createVenture(user1)
            |> withFirstPartner(user1)
            |> withAccount(~supporter=user1)
            |> withPartner(user2, ~supporters=[user1])
          );
        },
        (sessions, log) => {
          let (_user1, user2) = G.twoUserSessionsFromArray(sessions);
          testDataValidation(
            Validation.validateCustodianData,
            log |> constructState,
            Custodian.Data.{
              lastCustodianRemovalProcess: None,
              partnerId: user2.userId,
              partnerApprovalProcess: ProcessId.make(),
              accountIdx: AccountIndex.default,
            },
            Validation.BadData("partner approval process doesn't exist"),
          );
        },
      );
      F.withCached(
        ~scope="CustodianProposed",
        "when the account doesn't exist",
        () => G.withUserSessions(2),
        sessions => {
          let (user1, _user2) = G.twoUserSessionsFromArray(sessions);
          L.(createVenture(user1) |> withFirstPartner(user1));
        },
        (sessions, log) => {
          let (user1, _user2) = G.twoUserSessionsFromArray(sessions);
          let partnerApproval =
            log |> L.lastEvent |> Event.getPartnerAcceptedExn;
          testDataValidation(
            Validation.validateCustodianData,
            log |> constructState,
            Custodian.Data.{
              lastCustodianRemovalProcess: None,
              partnerId: user1.userId,
              partnerApprovalProcess: partnerApproval.processId,
              accountIdx: AccountIndex.default,
            },
            Validation.BadData("account doesn't exist"),
          );
        },
      );
      F.withCached(
        ~scope="CustodianProposed",
        "when lastCustodianRemovalProcess doesn't match",
        () => G.withUserSessions(2),
        sessions => {
          let (user1, user2) = G.twoUserSessionsFromArray(sessions);
          L.(
            createVenture(user1)
            |> withFirstPartner(user1)
            |> withAccount(~supporter=user1)
            |> withCustodian(user1, ~supporters=[user1])
            |> withPartner(user2, ~supporters=[user1])
          );
        },
        (sessions, log) => {
          let (user1, user2) = G.twoUserSessionsFromArray(sessions);
          let partnerApproval =
            log |> L.lastEvent |> Event.getPartnerAcceptedExn;
          let log =
            L.(
              log
              |> withCustodian(user2, ~supporters=[user1, user2])
              |> withCustodianRemoved(user2, ~supporters=[user1])
            );
          testDataValidation(
            Validation.validateCustodianData,
            log |> constructState,
            Custodian.Data.{
              lastCustodianRemovalProcess: None,
              partnerId: user2.userId,
              partnerApprovalProcess: partnerApproval.processId,
              accountIdx: AccountIndex.default,
            },
            Validation.BadData("Last removal doesn't match"),
          );
        },
      );
    });
  });
