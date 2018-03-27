open Jest;

open Expect;

open Event;

open PrimitiveTypes;

module Validation = Venture__Validation;

let () =
  describe("Validate AccountKeyChainUpdated", () => {
    let supporterId = UserId.fromString("supporter");
    let systemIssuer = Bitcoin.ECPair.makeRandom();
    let emptyState = Validation.makeState();
    let keyChain0 =
      AccountKeyChainUpdated.make(
        ~accountIndex=0,
        ~keyChainIndex=0,
        ~keyChain=AccountKeyChain.make(0, [])
      );
    let accountProposal =
      AccountCreation.Proposal.make(
        ~supporterId,
        ~policy=Policy.absolute,
        AccountCreation.Data.{accountIndex: 0, name: "Account"}
      );
    let accountCreation =
      AccountCreation.Acceptance.fromProposal(accountProposal);
    let validateWithState = (~keyChain=keyChain0, state) =>
      Validation.validateAccountKeyChainUpdated(keyChain, state, systemIssuer);
    test("The Account Exists", () =>
      expect((
        validateWithState(emptyState),
        validateWithState(
          emptyState
          |> Validation.apply(AccountCreationProposed(accountProposal))
          |> Validation.apply(AccountCreationAccepted(accountCreation))
        )
      ))
      |> toEqual((Validation.BadData("Account doesn't exist"), Validation.Ok))
    );
    test("The KeyChainIndex is in order", () => {
      let keyChain1 =
        AccountKeyChainUpdated.make(
          ~accountIndex=0,
          ~keyChainIndex=1,
          ~keyChain=AccountKeyChain.make(0, [])
        );
      let keyChain2 =
        AccountKeyChainUpdated.make(
          ~accountIndex=0,
          ~keyChainIndex=2,
          ~keyChain=AccountKeyChain.make(0, [])
        );
      let stateWithAccountAndKeyChain =
        emptyState
        |> Validation.apply(AccountCreationProposed(accountProposal))
        |> Validation.apply(AccountCreationAccepted(accountCreation))
        |> Validation.apply(AccountKeyChainUpdated(keyChain0));
      expect((
        validateWithState(~keyChain=keyChain2, stateWithAccountAndKeyChain),
        validateWithState(~keyChain=keyChain1, stateWithAccountAndKeyChain)
      ))
      |> toEqual((Validation.BadData("Bad KeyChainIndex"), Validation.Ok));
    });
    test("The CustodianKeyChains are the latest", () => {
      let masterKeyChain =
        Bitcoin.HDNode.make(
          systemIssuer,
          Utils.bufFromHex(
            "c8bce5e6dac6f931af17863878cce2ca3b704c61b3d775fe56881cc8ff3ab1cb"
          )
        );
      let custodianKeyChain0 =
        CustodianKeyChain.make(
          ~ventureId=VentureId.fromString("venture"),
          ~accountIndex=0,
          ~keyChainIndex=0,
          ~masterKeyChain
        )
        |> CustodianKeyChain.toPublicKeyChain;
      let custodianKeyChain1 =
        CustodianKeyChain.make(
          ~ventureId=VentureId.fromString("venture"),
          ~accountIndex=0,
          ~keyChainIndex=1,
          ~masterKeyChain
        )
        |> CustodianKeyChain.toPublicKeyChain;
      let custodianId = UserId.fromString("custodian");
      let stateWithAccountAndCustodianKeyChain =
        emptyState
        |> Validation.apply(AccountCreationProposed(accountProposal))
        |> Validation.apply(AccountCreationAccepted(accountCreation))
        |> Validation.apply(
             CustodianKeyChainUpdated(
               CustodianKeyChainUpdated.make(
                 ~partnerId=custodianId,
                 ~keyChain=custodianKeyChain0
               )
             )
           )
        |> Validation.apply(
             CustodianKeyChainUpdated(
               CustodianKeyChainUpdated.make(
                 ~partnerId=custodianId,
                 ~keyChain=custodianKeyChain1
               )
             )
           );
      let keyChain =
        AccountKeyChainUpdated.make(
          ~accountIndex=0,
          ~keyChainIndex=0,
          ~keyChain=
            AccountKeyChain.make(1, [(custodianId, custodianKeyChain0)])
        );
      let keyChain1 =
        AccountKeyChainUpdated.make(
          ~accountIndex=0,
          ~keyChainIndex=0,
          ~keyChain=
            AccountKeyChain.make(1, [(custodianId, custodianKeyChain1)])
        );
      expect((
        validateWithState(~keyChain, stateWithAccountAndCustodianKeyChain),
        validateWithState(
          ~keyChain=keyChain1,
          stateWithAccountAndCustodianKeyChain
        )
      ))
      |> toEqual((Validation.BadData("Bad CustodianKeyChain"), Validation.Ok));
    });
  });
