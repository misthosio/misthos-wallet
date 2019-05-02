open Jest;

open PrimitiveTypes;

open WalletTypes;

open Event;

open ValidationHelpers;

let () =
  describe(" AccountKeyChainIdentified", () => {
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

describe("AccountKeyChainActivated", () => {
  describe("when everything is fine", () => {
    let (user1, _user2) = G.twoUserSessions();
    let log =
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withAccount(~supporter=user1)
        |> withCustodian(user1, ~supporters=[user1])
        |> withCustodianKeyChain(user1)
        |> withAccountKeyChainIdentified
      );
    testValidationResult(
      log |> constructState,
      L.(log |> withAccountKeyChainActivated(user1) |> lastItem),
      Validation.Ok,
    );
  });
  describe("after a partner removal", () => {
    let (user1, user2) = G.twoUserSessions();
    let log =
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withAccount(~supporter=user1)
        |> withCustodian(user1, ~supporters=[user1])
        |> withCustodianKeyChain(user1)
        |> withAccountKeyChainIdentified
        |> withAccountKeyChainActivated(user1)
        |> withPartner(user2, ~supporters=[user1])
        |> withCustodian(user2, ~supporters=[user1, user2])
        |> withCustodianKeyChain(user2)
        |> withAccountKeyChainIdentified
        |> withAccountKeyChainActivated(user2)
        |> withAccountKeyChainActivated(user1)
        |> withPartnerRemoved(user2, ~supporters=[user1])
      );
    testValidationResult(
      log |> constructState,
      L.(log |> withAccountKeyChainActivated(~sequence=1, user1) |> lastItem),
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
        |> withAccountKeyChainIdentified
      );
    let activated: AccountKeyChainActivated.t =
      L.(log |> withAccountKeyChainActivated(user1) |> lastEvent)
      |> Event.getAccountKeyChainActivatedExn;
    testDataValidation(
      Validation.validateAccountKeyChainActivated |> withIssuer(user1),
      log |> constructState,
      AccountKeyChainActivated.{
        ...activated,
        accountIdx: 1 |> AccountIndex.fromInt,
      },
      Validation.BadData("Account doesn't exist"),
    );
  });
  describe("when the issuer doesn't match", () => {
    let (user1, user2) = G.twoUserSessions();
    let log =
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withAccount(~supporter=user1)
        |> withCustodian(user1, ~supporters=[user1])
        |> withCustodianKeyChain(user1)
        |> withAccountKeyChainIdentified
        |> withPartner(user2, ~supporters=[user1])
      );
    let activated: AccountKeyChainActivated.t =
      L.(log |> withAccountKeyChainActivated(user1) |> lastEvent)
      |> Event.getAccountKeyChainActivatedExn;
    testDataValidation(
      Validation.validateAccountKeyChainActivated |> withIssuer(user2),
      log |> constructState,
      activated,
      InvalidIssuer,
    );
  });
  describe("when the issuer is not a custodian", () => {
    let (user1, user2) = G.twoUserSessions();
    let log =
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withAccount(~supporter=user1)
        |> withCustodian(user1, ~supporters=[user1])
        |> withCustodianKeyChain(user1)
        |> withAccountKeyChainIdentified
        |> withPartner(user2, ~supporters=[user1])
      );
    let activated: AccountKeyChainActivated.t =
      L.(log |> withAccountKeyChainActivated(user2) |> lastEvent)
      |> Event.getAccountKeyChainActivatedExn;
    testDataValidation(
      Validation.validateAccountKeyChainActivated |> withIssuer(user2),
      log |> constructState,
      activated,
      BadData("Not a custodian"),
    );
  });
  describe("when the identifier is unknown", () => {
    let (user1, _user2) = G.twoUserSessions();
    let log =
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withAccount(~supporter=user1)
        |> withCustodian(user1, ~supporters=[user1])
        |> withCustodianKeyChain(user1)
        |> withAccountKeyChainIdentified
      );
    let activated: AccountKeyChainActivated.t =
      L.(log |> withAccountKeyChainActivated(user1) |> lastEvent)
      |> Event.getAccountKeyChainActivatedExn;
    testDataValidation(
      Validation.validateAccountKeyChainActivated |> withIssuer(user1),
      log |> constructState,
      {...activated, identifier: "bad"},
      BadData("Unknown AccountKeyChain identifier"),
    );
  });
  describe("when the sequence is not in order", () => {
    let (user1, _user2) = G.twoUserSessions();
    let log =
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withAccount(~supporter=user1)
        |> withCustodian(user1, ~supporters=[user1])
        |> withCustodianKeyChain(user1)
        |> withAccountKeyChainIdentified
      );
    let activated: AccountKeyChainActivated.t =
      L.(log |> withAccountKeyChainActivated(user1) |> lastEvent)
      |> Event.getAccountKeyChainActivatedExn;
    testDataValidation(
      Validation.validateAccountKeyChainActivated |> withIssuer(user1),
      log |> constructState,
      {...activated, sequence: 1},
      BadData("AccountKeyChain sequence out of order"),
    );
  });
});
