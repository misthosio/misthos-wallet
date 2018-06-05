open Jest;

open PrimitiveTypes;

open Event;

open ValidationHelpers;

let () = {
  describe("PartnerProposal", () => {
    F.withCached(
      ~scope="PartnerProposal",
      "when proposing another partner",
      () => G.withUserSessions(2),
      sessions => {
        let (user1, _user2) = G.twoUserSessionsFromArray(sessions);
        L.(createVenture(user1) |> withFirstPartner(user1));
      },
      (sessions, log) => {
        let (user1, user2) = G.twoUserSessionsFromArray(sessions);
        testValidationResult(
          log |> constructState,
          L.(
            log
            |> withPartnerProposed(~proposer=user1, ~prospect=user2)
            |> lastItem
          ),
          Validation.Ok,
        );
      },
    );
    F.withCached(
      ~scope="PartnerProposal",
      "when the prospect is already a partner",
      () => G.withUserSessions(2),
      sessions => {
        let (user1, user2) = G.twoUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
        );
      },
      (sessions, log) => {
        let (user1, user2) = G.twoUserSessionsFromArray(sessions);
        testValidationResult(
          log |> constructState,
          L.(
            log
            |> withPartnerProposed(~proposer=user2, ~prospect=user1)
            |> lastItem
          ),
          Validation.BadData("Partner already exists"),
        );
      },
    );
    describe("when the creator proposes themselves", () => {
      let user1 = G.userSession("user1" |> UserId.fromString);
      let log = L.createVenture(user1);
      testValidationResult(
        log |> constructState,
        L.(
          log
          |> withPartnerProposed(~proposer=user1, ~prospect=user1)
          |> lastItem
        ),
        Validation.Ok,
      );
    });
    F.withCached(
      ~scope="PartnerProposal",
      "when proposing a partner that was removed",
      () => G.withUserSessions(2),
      sessions => {
        let (user1, user2) = G.twoUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartnerRemoved(user2, ~supporters=[user1])
        );
      },
      (sessions, log) => {
        let (user1, user2) = G.twoUserSessionsFromArray(sessions);
        testValidationResult(
          log |> constructState,
          L.(
            log
            |> withPartnerProposed(~proposer=user1, ~prospect=user2)
            |> lastItem
          ),
          Validation.Ok,
        );
      },
    );
    F.withCached(
      ~scope="PartnerProposal",
      "when the partner was removed but the proposal doesn't show it",
      () => G.withUserSessions(3),
      sessions => {
        let (user1, user2, user3) = G.threeUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartner(user3, ~supporters=[user1, user2])
          |> withPartnerRemoved(user2, ~supporters=[user1, user3])
        );
      },
      (sessions, log) => {
        let (_user1, user2, user3) = G.threeUserSessionsFromArray(sessions);
        testValidationResult(
          log |> constructState,
          L.(
            log
            |> withPartnerProposed(
                 ~withLastRemoval=false,
                 ~proposer=user3,
                 ~prospect=user2,
               )
            |> lastItem
          ),
          Validation.BadData("Last removal doesn't match"),
        );
      },
    );
  });
  describe("PartnerRemovalProposal", () => {
    F.withCached(
      ~scope="PartnerRemovalProposal",
      "when proposing another partner",
      () => G.withUserSessions(2),
      sessions => {
        let (user1, user2) = G.twoUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
        );
      },
      (sessions, log) => {
        let (user1, user2) = G.twoUserSessionsFromArray(sessions);
        testValidationResult(
          log |> constructState,
          L.(
            log
            |> withPartnerRemovalProposed(
                 ~proposer=user1,
                 ~toBeRemoved=user2,
               )
            |> lastItem
          ),
          Validation.Ok,
        );
      },
    );
    describe("validatePartnerRemovalData", () => {
      F.withCached(
        ~scope="PartnerRemovalProposal",
        "when the prospect is not a partner",
        () => G.withUserSessions(2),
        sessions => {
          let (user1, _user2) = G.twoUserSessionsFromArray(sessions);
          L.(createVenture(user1) |> withFirstPartner(user1));
        },
        (sessions, log) => {
          let (_user1, user2) = G.twoUserSessionsFromArray(sessions);
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
        },
      );
      F.withCached(
        ~scope="PartnerRemovalProposal",
        "when lastPartnerProcess doesn't match",
        () => G.withUserSessions(2),
        sessions => {
          let (user1, user2) = G.twoUserSessionsFromArray(sessions);
          L.(
            createVenture(user1)
            |> withFirstPartner(user1)
            |> withPartner(user2, ~supporters=[user1])
          );
        },
        (sessions, log) => {
          let (_user1, user2) = G.twoUserSessionsFromArray(sessions);
          testDataValidation(
            Validation.validatePartnerRemovalData,
            log |> constructState,
            Partner.Removal.Data.{
              id: user2.userId,
              lastPartnerProcess: ProcessId.make(),
            },
            Validation.BadData("lastPartnerProcess doesn't match"),
          );
        },
      );
    });
  });
};
