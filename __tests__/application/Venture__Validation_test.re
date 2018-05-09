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
