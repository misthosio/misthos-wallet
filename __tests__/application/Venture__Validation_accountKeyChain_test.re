open Jest;

open PrimitiveTypes;

open WalletTypes;

open Event;

open ValidationHelpers;

let () =
  describe("AccountKeyChainIdentified", () => {
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
        L.(log |> withAccountKeyChainIdentified |> lastItem),
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
      let identified: AccountKeyChainIdentified.t =
        L.(log |> withAccountKeyChainIdentified |> lastEvent)
        |> Event.getAccountKeyChainIdentifiedExn;
      testDataValidation(
        Validation.validateAccountKeyChainIdentified |> withSystemIssuer,
        log |> constructState,
        AccountKeyChainIdentified.{
          keyChain: {
            ...identified.keyChain,
            accountIdx: 1 |> AccountIndex.fromInt,
          },
        },
        Validation.BadData("Account doesn't exist"),
      );
    });
    describe("when the AccountKeyChain is inconsistent", () => {
      let (user1, _user2) = G.twoUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
          |> withCustodian(user1, ~supporters=[user1])
          |> withCustodianKeyChain(user1)
        );
      let identified: AccountKeyChainIdentified.t =
        L.(log |> withAccountKeyChainIdentified |> lastEvent)
        |> Event.getAccountKeyChainIdentifiedExn;
      testDataValidation(
        Validation.validateAccountKeyChainIdentified |> withSystemIssuer,
        log |> constructState,
        AccountKeyChainIdentified.{
          keyChain: {
            ...identified.keyChain,
            identifier: "",
          },
        },
        Validation.BadData("Inconsistent AccountKeyChain"),
      );
    });
    describe("with an old custodian", () => {
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
          |> withAccountKeyChainIdentified
          |> withCustodianRemoved(user2, ~supporters=[user1])
          |> withPartnerRemoved(user2, ~supporters=[user1])
          |> withCustodianKeyChain(~keyChainIdx=1, user1)
        );
      testDataValidation(
        Validation.validateAccountKeyChainIdentified |> withSystemIssuer,
        log |> constructState,
        AccountKeyChainIdentified.{
          keyChain:
            AccountKeyChain.make(
              AccountIndex.default,
              [
                (
                  user1.userId,
                  G.custodianKeyChain(
                    ~ventureId=log |> L.ventureId,
                    ~keyChainIdx=1,
                    user1,
                  ),
                ),
                (
                  user2.userId,
                  G.custodianKeyChain(
                    ~ventureId=log |> L.ventureId,
                    ~keyChainIdx=0,
                    user2,
                  ),
                ),
              ],
            ),
        },
        Validation.BadData("Custodians aren't current"),
      );
    });
    describe("when a CustodianKeyChain is unknown", () => {
      let (user1, _user2) = G.twoUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
          |> withCustodian(user1, ~supporters=[user1])
          |> withCustodianKeyChain(user1)
        );
      testDataValidation(
        Validation.validateAccountKeyChainIdentified |> withSystemIssuer,
        log |> constructState,
        AccountKeyChainIdentified.{
          keyChain:
            AccountKeyChain.make(
              AccountIndex.default,
              [
                (
                  user1.userId,
                  G.custodianKeyChain(
                    ~ventureId=VentureId.make(),
                    ~keyChainIdx=1,
                    user1,
                  ),
                ),
              ],
            ),
        },
        Validation.BadData("Bad CustodianKeyChain"),
      );
    });
  });
