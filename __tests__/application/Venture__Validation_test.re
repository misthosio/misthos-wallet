open Jest;

open Expect;

open Event;

open PrimitiveTypes;

module G = Generators;

module E = G.Event;

module L = G.Log;

module Validation = Venture__Validation;

exception TestingInvalidSequence;

let constructState = log =>
  log
  |> L.reduce(
       (s, item) =>
         switch (s |. Validation.validate(item)) {
         | Ok => s |> Validation.apply(item)
         | _ => raise(TestingInvalidSequence)
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
  /* describe("Validate CustodianData", () => { */
  /*   let creatorId = UserId.fromString("creator.id"); */
  /*   let creatorKeyPair = Bitcoin.ECPair.makeRandom(); */
  /*   let creatorPubKey = creatorKeyPair |> Utils.publicKeyFromKeyPair; */
  /*   let createdEvent = */
  /*     VentureCreated.make( */
  /*       ~ventureName="test", */
  /*       ~creatorId, */
  /*       ~creatorPubKey, */
  /*       ~metaPolicy=Policy.unanimous, */
  /*       ~network=Network.Regtest, */
  /*     ); */
  /*   let partnerProposal = */
  /*     Event.makePartnerProposed( */
  /*       ~supporterId=creatorId, */
  /*       ~prospectId=creatorId, */
  /*       ~prospectPubKey=creatorPubKey, */
  /*       ~policy=Policy.unanimous, */
  /*     ) */
  /*     |> Event.getPartnerProposedExn; */
  /*   let state = */
  /*     Validation.makeState() */
  /*     |> Validation.apply(Event.VentureCreated(createdEvent)) */
  /*     |> Validation.apply(Event.PartnerProposed(partnerProposal)) */
  /*     |> Validation.apply( */
  /*          PartnerAccepted(Partner.Accepted.fromProposal(partnerProposal)), */
  /*        ); */
  /*   let accountIdx = AccountIndex.default; */
  /*   let custodianId = UserId.fromString("custodian.id"); */
  /*   let custodianKeyPair = Bitcoin.ECPair.makeRandom(); */
  /*   let custodianPubKey = custodianKeyPair |> Utils.publicKeyFromKeyPair; */
  /*   test("Fails if partner doesn't exist", () => */
  /*     state */
  /*     |> Validation.validateCustodianData({ */
  /*          partnerId: custodianId, */
  /*          accountIdx, */
  /*          partnerApprovalProcess: ProcessId.make(), */
  /*        }) */
  /*     |> expect */
  /*     |> toEqual( */
  /*          Validation.BadData("Partner with Id 'custodian.id' doesn't exist"), */
  /*        ) */
  /*   ); */
  /*   let custodianPartnerProposal = */
  /*     Event.makePartnerProposed( */
  /*       ~supporterId=creatorId, */
  /*       ~prospectId=custodianId, */
  /*       ~prospectPubKey=custodianPubKey, */
  /*       ~policy=Policy.unanimous, */
  /*     ) */
  /*     |> Event.getPartnerProposedExn; */
  /*   test("Succeeds if partner was proposed", () => */
  /*     state */
  /*     |> Validation.apply(Event.PartnerProposed(custodianPartnerProposal)) */
  /*     |> Validation.validateCustodianData({ */
  /*          partnerId: custodianId, */
  /*          accountIdx, */
  /*          partnerApprovalProcess: custodianPartnerProposal.processId, */
  /*        }) */
  /*     |> expect */
  /*     |> toEqual(Validation.Ok) */
  /*   ); */
  /* }); */
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
};
