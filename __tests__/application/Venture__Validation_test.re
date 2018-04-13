open Jest;

open Expect;

open Event;

open PrimitiveTypes;

open WalletTypes;

module Validation = Venture__Validation;

let () = {
  describe("Validate CustodianData", () => {
    let creatorId = UserId.fromString("creator.id");
    let creatorKeyPair = Bitcoin.ECPair.makeRandom();
    let creatorPubKey = creatorKeyPair |> Utils.publicKeyFromKeyPair;
    let createdEvent =
      VentureCreated.make(
        ~ventureName="test",
        ~creatorId,
        ~creatorPubKey,
        ~metaPolicy=Policy.absolute,
        ~network=Network.Regtest,
      );
    let Event.PartnerProposed(partnerProposal) =
      Event.makePartnerProposed(
        ~supporterId=creatorId,
        ~prospectId=creatorId,
        ~prospectPubKey=creatorPubKey,
        ~policy=Policy.absolute,
      );
    let state =
      Validation.makeState()
      |> Validation.apply(Event.VentureCreated(createdEvent))
      |> Validation.apply(Event.PartnerProposed(partnerProposal))
      |> Validation.apply(
           PartnerAccepted(Partner.Accepted.fromProposal(partnerProposal)),
         );
    let policy = Policy.absolute;
    let accountIdx = AccountIndex.default;
    let custodianId = UserId.fromString("custodian.id");
    let custodianKeyPair = Bitcoin.ECPair.makeRandom();
    let custodianPubKey = creatorKeyPair |> Utils.publicKeyFromKeyPair;
    test("Fails if partner doesn't exist", () =>
      state
      |> Validation.validateCustodianData(
           {partnerId: custodianId, accountIdx},
           None,
         )
      |> expect
      |> toEqual(
           Validation.BadData("Partner with Id 'custodian.id' doesn't exist"),
         )
    );
    let Event.PartnerProposed(custodianPartnerProposal) =
      Event.makePartnerProposed(
        ~supporterId=creatorId,
        ~prospectId=custodianId,
        ~prospectPubKey=custodianPubKey,
        ~policy=Policy.absolute,
      );
    test("Fails if partner was proposed", () =>
      state
      |> Validation.apply(Event.PartnerProposed(custodianPartnerProposal))
      |> Validation.validateCustodianData(
           {partnerId: custodianId, accountIdx},
           None,
         )
      |> expect
      |> toEqual(
           Validation.BadData("Partner with Id 'custodian.id' doesn't exist"),
         )
    );
    test("Fails if dependent process doesn't exist", () =>
      state
      |> Validation.validateCustodianData(
           {partnerId: custodianId, accountIdx},
           Some(custodianPartnerProposal.processId),
         )
      |> expect
      |> toEqual(Validation.DependencyNotMet)
    );
    test("Succeeds if partner was proposed and dependency is declared", () =>
      state
      |> Validation.apply(Event.PartnerProposed(custodianPartnerProposal))
      |> Validation.validateCustodianData(
           {partnerId: custodianId, accountIdx},
           Some(custodianPartnerProposal.processId),
         )
      |> expect
      |> toEqual(Validation.Ok)
    );
  });
  describe("Validate AccountKeyChainUpdated", () => {
    let supporterId = UserId.fromString("supporter");
    let systemIssuer = Bitcoin.ECPair.makeRandom();
    let emptyState = Validation.makeState();
    let keyChain0 =
      AccountKeyChainUpdated.make(
        ~keyChain=
          AccountKeyChain.make(
            AccountIndex.first,
            AccountKeyChainIndex.first,
            0,
            [],
          ),
      );
    let accountProposed =
      AccountCreation.Proposed.make(
        ~supporterId,
        ~policy=Policy.absolute,
        AccountCreation.Data.{
          accountIdx: AccountIndex.default,
          name: "Account",
        },
      );
    let accountCreation =
      AccountCreation.Accepted.fromProposal(accountProposed);
    let validateWithState = (~keyChain=keyChain0, state) =>
      Validation.validateAccountKeyChainUpdated(
        keyChain,
        state,
        systemIssuer,
      );
    test("The Account Exists", () =>
      expect((
        validateWithState(emptyState),
        validateWithState(
          emptyState
          |> Validation.apply(AccountCreationProposed(accountProposed))
          |> Validation.apply(AccountCreationAccepted(accountCreation)),
        ),
      ))
      |> toEqual((
           Validation.BadData("Account doesn't exist"),
           Validation.Ok,
         ))
    );
    test("The KeyChainIndex is in order", () => {
      let keyChain1 =
        AccountKeyChainUpdated.make(
          ~keyChain=
            AccountKeyChain.make(
              AccountIndex.default,
              AccountKeyChainIndex.first |> AccountKeyChainIndex.next,
              0,
              [],
            ),
        );
      let keyChain2 =
        AccountKeyChainUpdated.make(
          ~keyChain=
            AccountKeyChain.make(
              AccountIndex.default,
              AccountKeyChainIndex.first
              |> AccountKeyChainIndex.next
              |> AccountKeyChainIndex.next,
              0,
              [],
            ),
        );
      let stateWithAccountAndKeyChain =
        emptyState
        |> Validation.apply(AccountCreationProposed(accountProposed))
        |> Validation.apply(AccountCreationAccepted(accountCreation))
        |> Validation.apply(AccountKeyChainUpdated(keyChain0));
      expect((
        validateWithState(~keyChain=keyChain2, stateWithAccountAndKeyChain),
        validateWithState(~keyChain=keyChain1, stateWithAccountAndKeyChain),
      ))
      |> toEqual((Validation.BadData("Bad KeyChainIndex"), Validation.Ok));
    });
    test("The CustodianKeyChains are the latest", () => {
      let masterKeyChain =
        Bitcoin.HDNode.make(
          systemIssuer,
          Utils.bufFromHex(
            "c8bce5e6dac6f931af17863878cce2ca3b704c61b3d775fe56881cc8ff3ab1cb",
          ),
        );
      let custodianKeyChain0 =
        CustodianKeyChain.make(
          ~ventureId=VentureId.fromString("venture"),
          ~accountIdx=AccountIndex.default,
          ~keyChainIdx=CustodianKeyChainIndex.first,
          ~masterKeyChain,
        )
        |> CustodianKeyChain.toPublicKeyChain;
      let custodianKeyChain1 =
        CustodianKeyChain.make(
          ~ventureId=VentureId.fromString("venture"),
          ~accountIdx=AccountIndex.default,
          ~keyChainIdx=
            CustodianKeyChainIndex.first |> CustodianKeyChainIndex.next,
          ~masterKeyChain,
        )
        |> CustodianKeyChain.toPublicKeyChain;
      let custodianId = UserId.fromString("custodian");
      let stateWithAccountAndCustodianKeyChain =
        emptyState
        |> Validation.apply(AccountCreationProposed(accountProposed))
        |> Validation.apply(AccountCreationAccepted(accountCreation))
        |> Validation.apply(
             CustodianKeyChainUpdated(
               CustodianKeyChainUpdated.make(
                 ~partnerId=custodianId,
                 ~keyChain=custodianKeyChain0,
               ),
             ),
           )
        |> Validation.apply(
             CustodianKeyChainUpdated(
               CustodianKeyChainUpdated.make(
                 ~partnerId=custodianId,
                 ~keyChain=custodianKeyChain1,
               ),
             ),
           );
      let keyChain =
        AccountKeyChainUpdated.make(
          ~keyChain=
            AccountKeyChain.make(
              AccountIndex.default,
              AccountKeyChainIndex.first,
              1,
              [(custodianId, custodianKeyChain0)],
            ),
        );
      let keyChain1 =
        AccountKeyChainUpdated.make(
          ~keyChain=
            AccountKeyChain.make(
              AccountIndex.default,
              AccountKeyChainIndex.first,
              1,
              [(custodianId, custodianKeyChain1)],
            ),
        );
      expect((
        validateWithState(~keyChain, stateWithAccountAndCustodianKeyChain),
        validateWithState(
          ~keyChain=keyChain1,
          stateWithAccountAndCustodianKeyChain,
        ),
      ))
      |> toEqual((
           Validation.BadData("Bad CustodianKeyChain"),
           Validation.Ok,
         ));
    });
  });
};
