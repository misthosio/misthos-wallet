open Jest;

open WalletTypes;

open Event;

open ValidationHelpers;

let () =
  describe("AccountKeyChainUpdate", () => {
    describe("when everything is fine", () => {
      let (user1, _user2) = G.twoUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
          |> withCustodian(user1, ~supporters=[user1])
          |> withCustodianKeyChain(user1)
        );
      testValidationResult(
        log |> constructState,
        L.(log |> withAccountKeyChain |> lastItem),
        Validation.Ok,
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
          |> withCustodianKeyChain(user1)
        );
      let {keyChain}: AccountKeyChainUpdated.t =
        L.(log |> withAccountKeyChain |> lastEvent)
        |> Event.getAccountKeyChainUpdatedExn;
      testDataValidation(
        Validation.validateAccountKeyChainUpdated |> withSystemIssuer,
        log |> constructState,
        AccountKeyChainUpdated.{
          keyChain: {
            ...keyChain,
            accountIdx: 1 |> AccountIndex.fromInt,
          },
        },
        Validation.BadData("Account doesn't exist"),
      );
    });
    describe("when the AccountKeyChainIndex is wrong", () => {
      let (user1, _user2) = G.twoUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
          |> withCustodian(user1, ~supporters=[user1])
          |> withCustodianKeyChain(user1)
        );
      let {keyChain}: AccountKeyChainUpdated.t =
        L.(log |> withAccountKeyChain |> lastEvent)
        |> Event.getAccountKeyChainUpdatedExn;
      testDataValidation(
        Validation.validateAccountKeyChainUpdated |> withSystemIssuer,
        log |> constructState,
        AccountKeyChainUpdated.{
          keyChain: {
            ...keyChain,
            keyChainIdx: 1 |> AccountKeyChainIndex.fromInt,
          },
        },
        Validation.BadData("Bad AccountKeyChainIndex"),
      );
    });
    describe("when the custodian list is wrong", () => {
      let (user1, user2) = G.twoUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
          |> withCustodian(user1, ~supporters=[user1])
          |> withCustodianKeyChain(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withCustodian(user2, ~supporters=[user1, user2])
          |> withCustodianKeyChain(user2)
        );
      let {keyChain}: AccountKeyChainUpdated.t =
        L.(log |> withAccountKeyChain |> lastEvent)
        |> Event.getAccountKeyChainUpdatedExn;
      testDataValidation(
        Validation.validateAccountKeyChainUpdated |> withSystemIssuer,
        log |> constructState,
        AccountKeyChainUpdated.{
          keyChain: {
            ...keyChain,
            custodianKeyChains: [keyChain.custodianKeyChains |> List.hd],
          },
        },
        Validation.BadData("Wrong custodians"),
      );
    });
    describe("when a custodian key chain is wrong", () => {
      let (user1, user2) = G.twoUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
          |> withCustodian(user1, ~supporters=[user1])
          |> withCustodianKeyChain(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartnerRemoved(user2, ~supporters=[user1])
          |> withCustodianKeyChain(~keyChainIdx=1, user1)
        );
      let {keyChain}: AccountKeyChainUpdated.t =
        L.(log |> withAccountKeyChain |> lastEvent)
        |> Event.getAccountKeyChainUpdatedExn;
      testDataValidation(
        Validation.validateAccountKeyChainUpdated |> withSystemIssuer,
        log |> constructState,
        AccountKeyChainUpdated.{
          keyChain: {
            ...keyChain,
            custodianKeyChains: [
              (
                user1.userId,
                G.custodianKeyChain(
                  ~ventureId=log |> L.ventureId,
                  ~keyChainIdx=0,
                  user1,
                ),
              ),
              ...keyChain.custodianKeyChains
                 |> List.remove_assoc(user1.userId),
            ],
          },
        },
        Validation.BadData("Bad CustodianKeyChain"),
      );
    });
  });
