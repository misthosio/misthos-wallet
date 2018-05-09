open Jest;

open PrimitiveTypes;

open Event;

open ValidationHelpers;

let () = {
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
};
