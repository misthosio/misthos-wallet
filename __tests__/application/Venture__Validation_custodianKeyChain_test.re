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
/* describe("Validate AccountKeyChainUpdated", () => { */
/*   let supporterId = UserId.fromString("supporter"); */
/*   let systemIssuer = Bitcoin.ECPair.makeRandom(); */
/*   let emptyState = Validation.makeState(); */
/*   let keyChain0 = */
/*     AccountKeyChainUpdated.make( */
/*       ~keyChain= */
/*         AccountKeyChain.make( */
/*           AccountIndex.first, */
/*           AccountKeyChainIndex.first, */
/*           0, */
/*           [], */
/*         ), */
/*     ); */
/*   let accountProposed = */
/*     AccountCreation.Proposed.make( */
/*       ~supporterId, */
/*       ~policy=Policy.unanimous, */
/*       AccountCreation.Data.{ */
/*         accountIdx: AccountIndex.default, */
/*         name: "Account", */
/*       }, */
/*     ); */
/*   let accountCreation = */
/*     AccountCreation.Accepted.fromProposal(accountProposed); */
/*   let validateWithState = (~keyChain=keyChain0, state) => */
/*     Validation.validateAccountKeyChainUpdated( */
/*       keyChain, */
/*       state, */
/*       systemIssuer, */
/*     ); */
/*   test("The Account Exists", () => */
/*     expect(( */
/*       validateWithState(emptyState), */
/*       validateWithState( */
/*         emptyState */
/*         |> Validation.apply(AccountCreationProposed(accountProposed)) */
/*         |> Validation.apply(AccountCreationAccepted(accountCreation)), */
/*       ), */
/*     )) */
/*     |> toEqual(( */
/*          Validation.BadData("Account doesn't exist"), */
/*          Validation.Ok, */
/*        )) */
/*   ); */
/*   test("The KeyChainIndex is in order", () => { */
/*     let keyChain1 = */
/*       AccountKeyChainUpdated.make( */
/*         ~keyChain= */
/*           AccountKeyChain.make( */
/*             AccountIndex.default, */
/*             AccountKeyChainIndex.first |> AccountKeyChainIndex.next, */
/*             0, */
/*             [], */
/*           ), */
/*       ); */
/*     let keyChain2 = */
/*       AccountKeyChainUpdated.make( */
/*         ~keyChain= */
/*           AccountKeyChain.make( */
/*             AccountIndex.default, */
/*             AccountKeyChainIndex.first */
/*             |> AccountKeyChainIndex.next */
/*             |> AccountKeyChainIndex.next, */
/*             0, */
/*             [], */
/*           ), */
/*       ); */
/*     let stateWithAccountAndKeyChain = */
/*       emptyState */
/*       |> Validation.apply(AccountCreationProposed(accountProposed)) */
/*       |> Validation.apply(AccountCreationAccepted(accountCreation)) */
/*       |> Validation.apply(AccountKeyChainUpdated(keyChain0)); */
/*     expect(( */
/*       validateWithState(~keyChain=keyChain2, stateWithAccountAndKeyChain), */
/*       validateWithState(~keyChain=keyChain1, stateWithAccountAndKeyChain), */
/*     )) */
/*     |> toEqual((Validation.BadData("Bad KeyChainIndex"), Validation.Ok)); */
/*   }); */
/*   test("The CustodianKeyChains are the latest", () => { */
/*     let masterKeyChain = */
/*       Bitcoin.HDNode.make( */
/*         systemIssuer, */
/*         Utils.bufFromHex( */
/*           "c8bce5e6dac6f931af17863878cce2ca3b704c61b3d775fe56881cc8ff3ab1cb", */
/*         ), */
/*       ); */
/*     let custodianKeyChain0 = */
/*       CustodianKeyChain.make( */
/*         ~ventureId=VentureId.fromString("venture"), */
/*         ~accountIdx=AccountIndex.default, */
/*         ~keyChainIdx=CustodianKeyChainIndex.first, */
/*         ~masterKeyChain, */
/*       ) */
/*       |> CustodianKeyChain.toPublicKeyChain; */
/*     let custodianKeyChain1 = */
/*       CustodianKeyChain.make( */
/*         ~ventureId=VentureId.fromString("venture"), */
/*         ~accountIdx=AccountIndex.default, */
/*         ~keyChainIdx= */
/*           CustodianKeyChainIndex.first |> CustodianKeyChainIndex.next, */
/*         ~masterKeyChain, */
/*       ) */
/*       |> CustodianKeyChain.toPublicKeyChain; */
/*     let custodianId = UserId.fromString("custodian"); */
/*     let stateWithAccountAndCustodianKeyChain = */
/*       emptyState */
/*       |> Validation.apply(AccountCreationProposed(accountProposed)) */
/*       |> Validation.apply(AccountCreationAccepted(accountCreation)) */
/*       |> Validation.apply( */
/*            CustodianKeyChainUpdated( */
/*              CustodianKeyChainUpdated.make( */
/*                ~custodianApprovalProcess=ProcessId.make(), */
/*                ~partnerId=custodianId, */
/*                ~keyChain=custodianKeyChain0, */
/*              ), */
/*            ), */
/*          ) */
/*       |> Validation.apply( */
/*            CustodianKeyChainUpdated( */
/*              CustodianKeyChainUpdated.make( */
/*                ~custodianApprovalProcess=ProcessId.make(), */
/*                ~partnerId=custodianId, */
/*                ~keyChain=custodianKeyChain1, */
/*              ), */
/*            ), */
/*          ); */
/*     let keyChain = */
/*       AccountKeyChainUpdated.make( */
/*         ~keyChain= */
/*           AccountKeyChain.make( */
/*             AccountIndex.default, */
/*             AccountKeyChainIndex.first, */
/*             1, */
/*             [(custodianId, custodianKeyChain0)], */
/*           ), */
/*       ); */
/*     let keyChain1 = */
/*       AccountKeyChainUpdated.make( */
/*         ~keyChain= */
/*           AccountKeyChain.make( */
/*             AccountIndex.default, */
/*             AccountKeyChainIndex.first, */
/*             1, */
/*             [(custodianId, custodianKeyChain1)], */
/*           ), */
/*       ); */
/*     expect(( */
/*       validateWithState(~keyChain, stateWithAccountAndCustodianKeyChain), */
/*       validateWithState( */
/*         ~keyChain=keyChain1, */
/*         stateWithAccountAndCustodianKeyChain, */
/*       ), */
/*     )) */
/*     |> toEqual(( */
/*          Validation.BadData("Bad CustodianKeyChain"), */
/*          Validation.Ok, */
/*        )); */
/*   }); */
/* }); */
