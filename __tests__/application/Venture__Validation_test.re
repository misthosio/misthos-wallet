open Jest;

open Expect;

open Event;

open PrimitiveTypes;

open WalletTypes;

module G = Generators;

module E = G.Event;

module L = G.Log;

module Validation = Venture__Validation;

exception TestingInvalidSequence(string);

let constructState = log =>
  log
  |> L.reduce(
       (s, item) =>
         switch (s |. Validation.validate(item)) {
         | Ok => s |> Validation.apply(item)
         | bad =>
           raise(TestingInvalidSequence(bad |> Validation.resultToString))
         },
       Validation.make(),
     );

let testValidationResult = (state, item, expected) => {
  let description = expected |> Validation.resultToString;
  test("valdation should return '" ++ description ++ "'", () =>
    expect(item |> Validation.validate(state) |> Validation.resultToString)
    |> toEqual(description)
  );
};

let testDataValidation =
    (
      dataValidation: ('a, Validation.t) => Validation.result,
      state,
      data: 'a,
      expected,
    ) => {
  let description = expected |> Validation.resultToString;
  test("valdation should return '" ++ description ++ "'", () =>
    expect(state |> dataValidation(data) |> Validation.resultToString)
    |> toEqual(description)
  );
};

let () = {
  describe("CreateVenture", () => {
    describe("as first event", () => {
      let user1 = G.userSession("user1" |> UserId.fromString);
      let log = L.createVenture(user1);
      testValidationResult(
        Validation.make(),
        log |> L.lastItem,
        Validation.Ok,
      );
    });
    describe("not as first event", () => {
      let (user1, user2) = G.twoUserSessions();
      let log = L.createVenture(user1);
      testValidationResult(
        log |> constructState,
        L.(
          log
          |> appendEvent(
               user2.issuerKeyPair,
               VentureCreated(E.createVenture(user2)),
             )
          |> lastItem
        ),
        Validation.BadData("Venture is already created"),
      );
    });
  });
  describe("Any proposal type", () => {
    describe("when submitting the identical proposal twice", () => {
      let (user1, user2) = G.twoUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartnerProposed(~supporter=user1, ~prospect=user2)
        );
      testValidationResult(
        log |> constructState,
        L.(log |> lastItem),
        Validation.Ignore,
      );
    });
    describe("with the wrong policy", () => {
      let (user1, user2) = G.twoUserSessions();
      let log = L.(createVenture(user1) |> withFirstPartner(user1));
      testValidationResult(
        log |> constructState,
        L.(
          log
          |> withPartnerProposed(
               ~policy=Policy.unanimousMinusOne,
               ~supporter=user1,
               ~prospect=user2,
             )
          |> lastItem
        ),
        Validation.PolicyMissmatch,
      );
    });
    describe("when the supporter is a non-partner", () => {
      let (user1, user2, user3) = G.threeUserSessions();
      let log = L.(createVenture(user1) |> withFirstPartner(user1));
      testValidationResult(
        log |> constructState,
        L.(
          log
          |> withPartnerProposed(~supporter=user2, ~prospect=user3)
          |> lastItem
        ),
        Validation.InvalidIssuer,
      );
    });
    describe("when the supporter is not the signer", () => {
      let (user1, user2, user3) = G.threeUserSessions();
      let log = L.(createVenture(user1) |> withFirstPartner(user1));
      testValidationResult(
        log |> constructState,
        L.(
          log
          |> withPartnerProposed(
               ~issuer=user1.issuerKeyPair,
               ~supporter=user2,
               ~prospect=user3,
             )
          |> lastItem
        ),
        Validation.InvalidIssuer,
      );
    });
    describe("when the proposal was already submitted by this partner", () => {
      let (user1, user2) = G.twoUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartnerProposed(~supporter=user1, ~prospect=user2)
        );
      testValidationResult(
        log |> constructState,
        L.(
          log
          |> withPartnerProposed(~supporter=user1, ~prospect=user2)
          |> lastItem
        ),
        Validation.BadData("This proposal already exists"),
      );
    });
    describe("when the same proposal was already made by another partner", () => {
      let (user1, user2, user3) = G.threeUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartnerProposed(~supporter=user1, ~prospect=user3)
        );
      testValidationResult(
        log |> constructState,
        L.(
          log
          |> withPartnerProposed(~supporter=user2, ~prospect=user3)
          |> lastItem
        ),
        Validation.Ok,
      );
    });
  });
  describe("Any rejection type", () => {
    describe("when the process is unknown", () => {
      let (user1, user2, user3) = G.threeUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartnerProposed(~supporter=user1, ~prospect=user3)
        );
      testValidationResult(
        log |> constructState,
        L.(
          log
          |> appendEvent(
               user2.issuerKeyPair,
               Event.makePartnerRejected(
                 ~processId=ProcessId.make(),
                 ~rejectorId=user2.userId,
               ),
             )
          |> lastItem
        ),
        Validation.UnknownProcessId,
      );
    });
    describe("when the rejector is not a partner", () => {
      let (user1, user2, user3) = G.threeUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartnerProposed(~supporter=user1, ~prospect=user3)
        );
      let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
      testValidationResult(
        log |> constructState,
        L.(log |> withPartnerRejected(user3, proposal) |> lastItem),
        Validation.InvalidIssuer,
      );
    });
    describe("when the rejector is not the signer", () => {
      let (user1, user2, user3) = G.threeUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartnerProposed(~supporter=user1, ~prospect=user3)
        );
      let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
      testValidationResult(
        log |> constructState,
        L.(
          log
          |> withPartnerRejected(
               ~issuer=user1.issuerKeyPair,
               user2,
               proposal,
             )
          |> lastItem
        ),
        Validation.InvalidIssuer,
      );
    });
    describe("when the rejection has already been submitted", () => {
      let (user1, user2, user3, user4) = G.fourUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartner(user3, ~supporters=[user1, user2])
          |> withPartnerProposed(~supporter=user1, ~prospect=user4)
        );
      let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
      let log = log |> L.withPartnerRejected(user2, proposal);
      testValidationResult(
        log |> constructState,
        L.(log |> lastItem),
        Validation.Ignore,
      );
    });
    describe("when the rejector has already endorsed", () => {
      let (user1, user2, user3, user4) = G.fourUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartner(user3, ~supporters=[user1, user2])
          |> withPartnerProposed(~supporter=user1, ~prospect=user4)
        );
      let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
      let log = log |> L.withPartnerEndorsed(user2, proposal);
      testValidationResult(
        log |> constructState,
        L.(log |> withPartnerRejected(user2, proposal) |> lastItem),
        Validation.AlreadyEndorsed,
      );
    });
    describe("when the rejection is fine", () => {
      let (user1, user2, user3) = G.threeUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartnerProposed(~supporter=user1, ~prospect=user3)
        );
      let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
      testValidationResult(
        log |> constructState,
        L.(log |> withPartnerRejected(user2, proposal) |> lastItem),
        Validation.Ok,
      );
    });
  });
  describe("Any endorsement type", () => {
    describe("when the process is unknown", () => {
      let (user1, user2, user3) = G.threeUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartnerProposed(~supporter=user1, ~prospect=user3)
        );
      testValidationResult(
        log |> constructState,
        L.(
          log
          |> appendEvent(
               user2.issuerKeyPair,
               Event.makePartnerEndorsed(
                 ~processId=ProcessId.make(),
                 ~supporterId=user2.userId,
               ),
             )
          |> lastItem
        ),
        Validation.UnknownProcessId,
      );
    });
    describe("when the supporter is not a partner", () => {
      let (user1, user2, user3) = G.threeUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartnerProposed(~supporter=user1, ~prospect=user3)
        );
      let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
      testValidationResult(
        log |> constructState,
        L.(log |> withPartnerEndorsed(user3, proposal) |> lastItem),
        Validation.InvalidIssuer,
      );
    });
    describe("when the supporter is not the signer", () => {
      let (user1, user2, user3) = G.threeUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartnerProposed(~supporter=user1, ~prospect=user3)
        );
      let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
      testValidationResult(
        log |> constructState,
        L.(
          log
          |> withPartnerEndorsed(
               ~issuer=user1.issuerKeyPair,
               user2,
               proposal,
             )
          |> lastItem
        ),
        Validation.InvalidIssuer,
      );
    });
    describe("when the endorsement has already been submitted", () => {
      let (user1, user2, user3, user4) = G.fourUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartner(user3, ~supporters=[user1, user2])
          |> withPartnerProposed(~supporter=user1, ~prospect=user4)
        );
      let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
      let log = log |> L.withPartnerEndorsed(user2, proposal);
      testValidationResult(
        log |> constructState,
        L.(log |> lastItem),
        Validation.Ignore,
      );
    });
    describe("when the endorsement is fine", () => {
      let (user1, user2, user3) = G.threeUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartnerProposed(~supporter=user1, ~prospect=user3)
        );
      let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
      testValidationResult(
        log |> constructState,
        L.(log |> withPartnerEndorsed(user2, proposal) |> lastItem),
        Validation.Ok,
      );
    });
  });
  describe("PartnerProposal", () => {
    describe("when proposing another partner", () => {
      let (user1, user2) = G.twoUserSessions();
      let log = L.(createVenture(user1) |> withFirstPartner(user1));
      testValidationResult(
        log |> constructState,
        L.(
          log
          |> withPartnerProposed(~supporter=user1, ~prospect=user2)
          |> lastItem
        ),
        Validation.Ok,
      );
    });
    describe("when the prospect is already a partner", () => {
      let (user1, user2) = G.twoUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
        );
      testValidationResult(
        log |> constructState,
        L.(
          log
          |> withPartnerProposed(~supporter=user2, ~prospect=user1)
          |> lastItem
        ),
        Validation.BadData("Partner already exists"),
      );
    });
    describe("when the creator proposes themselves", () => {
      let user1 = G.userSession("user1" |> UserId.fromString);
      let log = L.createVenture(user1);
      testValidationResult(
        log |> constructState,
        L.(
          log
          |> withPartnerProposed(~supporter=user1, ~prospect=user1)
          |> lastItem
        ),
        Validation.Ok,
      );
    });
    describe("when proposing a partner that was removed", () => {
      let (user1, user2) = G.twoUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartnerRemoved(user2, ~supporters=[user1])
        );
      testValidationResult(
        log |> constructState,
        L.(
          log
          |> withPartnerProposed(~supporter=user1, ~prospect=user2)
          |> lastItem
        ),
        Validation.Ok,
      );
    });
    describe(
      "when the partner was removed but the proposal doesn't show it", () => {
      let (user1, user2, user3) = G.threeUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartner(user3, ~supporters=[user1, user2])
          |> withPartnerRemoved(user2, ~supporters=[user1, user3])
        );
      testValidationResult(
        log |> constructState,
        L.(
          log
          |> withPartnerProposed(
               ~withLastRemoval=false,
               ~supporter=user3,
               ~prospect=user2,
             )
          |> lastItem
        ),
        Validation.BadData("Last removal doesn't match"),
      );
    });
  });
  describe("PartnerRemovalProposal", () => {
    describe("when proposing another partner", () => {
      let (user1, user2) = G.twoUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
        );
      testValidationResult(
        log |> constructState,
        L.(
          log
          |> withPartnerRemovalProposed(~supporter=user1, ~toBeRemoved=user2)
          |> lastItem
        ),
        Validation.Ok,
      );
    });
    describe("validatePartnerRemovalData", () => {
      describe("when the prospect is not a partner", () => {
        let (user1, user2) = G.twoUserSessions();
        let log = L.(createVenture(user1) |> withFirstPartner(user1));
        testDataValidation(
          Validation.validatePartnerRemovalData,
          log |> constructState,
          Partner.Removal.Data.{
            id: user2.userId,
            lastPartnerProcess: ProcessId.make(),
          },
          Validation.BadData(
            "Partner with Id '"
            ++ UserId.toString(user2.userId)
            ++ "' doesn't exist",
          ),
        );
      });
      describe("when lastPartnerProcess doesn't match", () => {
        let (user1, user2) = G.twoUserSessions();
        let log =
          L.(
            createVenture(user1)
            |> withFirstPartner(user1)
            |> withPartner(user2, ~supporters=[user1])
          );
        testDataValidation(
          Validation.validatePartnerRemovalData,
          log |> constructState,
          Partner.Removal.Data.{
            id: user2.userId,
            lastPartnerProcess: ProcessId.make(),
          },
          Validation.BadData("lastPartnerProcess doesn't match"),
        );
      });
    });
  });
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
};
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
