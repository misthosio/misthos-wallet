open Jest;

open PrimitiveTypes;

open WalletTypes;

open Event;

open ValidationHelpers;

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
};
