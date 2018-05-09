open Jest;

open PrimitiveTypes;

open WalletTypes;

open Event;

open ValidationHelpers;

let () =
  describe("CustodianProposed", () => {
    describe("when proposing a custodian", () => {
      let (user1, user2) = G.twoUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
          |> withCustodian(user1, ~supporters=[user1])
          |> withPartner(user2, ~supporters=[user1])
        );
      testValidationResult(
        log |> constructState,
        L.(
          log
          |> withCustodianProposed(~supporter=user1, ~custodian=user2)
          |> lastItem
        ),
        Validation.Ok,
      );
    });
    describe("when proposing a custodian after removal", () => {
      let (user1, user2) = G.twoUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
          |> withCustodian(user1, ~supporters=[user1])
          |> withPartner(user2, ~supporters=[user1])
          |> withCustodian(user2, ~supporters=[user1, user2])
          |> withCustodianRemoved(user2, ~supporters=[user1, user2])
        );
      testValidationResult(
        log |> constructState,
        L.(
          log
          |> withCustodianProposed(~supporter=user1, ~custodian=user2)
          |> lastItem
        ),
        Validation.Ok,
      );
    });
    describe("validateCustodianData", () => {
      describe("when the custodian is not a partner", () => {
        let (user1, user2, user3) = G.threeUserSessions();
        let log =
          L.(
            createVenture(user1)
            |> withFirstPartner(user1)
            |> withAccount(~supporter=user1)
            |> withPartner(user2, ~supporters=[user1])
          );
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
      });
      describe("when the partner approval process reference is wrong", () => {
        let (user1, user2) = G.twoUserSessions();
        let log =
          L.(
            createVenture(user1)
            |> withFirstPartner(user1)
            |> withAccount(~supporter=user1)
            |> withPartner(user2, ~supporters=[user1])
          );
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
      });
      describe("when the account doesn't exist", () => {
        let (user1, _user2) = G.twoUserSessions();
        let log = L.(createVenture(user1) |> withFirstPartner(user1));
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
      });
      describe("when lastCustodianRemovalProcess doesn't match", () => {
        let (user1, user2) = G.twoUserSessions();
        let log =
          L.(
            createVenture(user1)
            |> withFirstPartner(user1)
            |> withAccount(~supporter=user1)
            |> withCustodian(user1, ~supporters=[user1])
            |> withPartner(user2, ~supporters=[user1])
          );
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
      });
    });
  });
