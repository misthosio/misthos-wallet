open Jest;

open PrimitiveTypes;

open WalletTypes;

open Event;

open ValidationHelpers;

let () =
  describe("CustodianKeyChainUpdate", () => {
    describe("when everything is fine", () => {
      let (user1, _user2) = G.twoUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
          |> withCustodian(user1, ~supporters=[user1])
        );
      testValidationResult(
        log |> constructState,
        L.(log |> withCustodianKeyChain(user1) |> lastItem),
        Validation.Ok,
      );
    });
    describe("when the signer doesn't match the custodianId", () => {
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
        L.(log |> withCustodianKeyChain(~issuer=user2, user1) |> lastItem),
        Validation.InvalidIssuer,
      );
    });
    describe("when the custodianApprovalProcess doesn't exist", () => {
      let (user1, _user2) = G.twoUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
          |> withCustodian(user1, ~supporters=[user1])
        );
      testDataValidation(
        Validation.validateCustodianKeyChainUpdated |> withIssuer(user1),
        log |> constructState,
        CustodianKeyChainUpdated.{
          custodianApprovalProcess: ProcessId.make(),
          custodianId: user1.userId,
          keyChain:
            G.custodianKeyChain(
              ~ventureId=log |> L.ventureId,
              ~keyChainIdx=0,
              user1,
            ),
        },
        Validation.BadData("Bad custodianApprovalProcess"),
      );
    });
    describe("when the custodianApprovalProcess isn't completed", () => {
      let (user1, _user2) = G.twoUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
          |> withCustodianProposed(~supporter=user1, ~custodian=user1)
        );
      let proposal = log |> L.lastEvent |> Event.getCustodianProposedExn;
      testDataValidation(
        Validation.validateCustodianKeyChainUpdated |> withIssuer(user1),
        log |> constructState,
        CustodianKeyChainUpdated.{
          custodianApprovalProcess: proposal.processId,
          custodianId: user1.userId,
          keyChain:
            G.custodianKeyChain(
              ~ventureId=log |> L.ventureId,
              ~keyChainIdx=0,
              user1,
            ),
        },
        Validation.BadData("Bad custodianApprovalProcess"),
      );
    });
    describe("when the custodian approval process is for another user", () => {
      let (user1, user2) = G.twoUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
          |> withCustodian(user1, ~supporters=[user1])
        );
      let accepted = log |> L.lastEvent |> Event.getCustodianAcceptedExn;
      let log = log |> L.withPartner(user2, ~supporters=[user1]);
      testDataValidation(
        Validation.validateCustodianKeyChainUpdated |> withIssuer(user2),
        log |> constructState,
        CustodianKeyChainUpdated.{
          custodianApprovalProcess: accepted.processId,
          custodianId: user2.userId,
          keyChain:
            G.custodianKeyChain(
              ~ventureId=log |> L.ventureId,
              ~keyChainIdx=0,
              user1,
            ),
        },
        Validation.BadData("CustodianApprovalProcess is for another partner"),
      );
    });
    describe("when the account doesn't exist", () => {
      let (user1, _user2) = G.twoUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
          |> withCustodian(user1, ~supporters=[user1])
        );
      let accepted = log |> L.lastEvent |> Event.getCustodianAcceptedExn;
      testDataValidation(
        Validation.validateCustodianKeyChainUpdated |> withIssuer(user1),
        log |> constructState,
        CustodianKeyChainUpdated.{
          custodianApprovalProcess: accepted.processId,
          custodianId: user1.userId,
          keyChain:
            G.custodianKeyChain(
              ~accountIdx=1 |> AccountIndex.fromInt,
              ~ventureId=log |> L.ventureId,
              ~keyChainIdx=0,
              user1,
            ),
        },
        Validation.BadData("Account doesn't exist"),
      );
    });
    describe("when the key chain index isn't in order", () => {
      let (user1, _user2) = G.twoUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
          |> withCustodian(user1, ~supporters=[user1])
        );
      let accepted = log |> L.lastEvent |> Event.getCustodianAcceptedExn;
      testDataValidation(
        Validation.validateCustodianKeyChainUpdated |> withIssuer(user1),
        log |> constructState,
        CustodianKeyChainUpdated.{
          custodianApprovalProcess: accepted.processId,
          custodianId: user1.userId,
          keyChain:
            G.custodianKeyChain(
              ~ventureId=log |> L.ventureId,
              ~keyChainIdx=1,
              user1,
            ),
        },
        Validation.BadData("CustodianKeyChainIndex isn't in order"),
      );
    });
  });
