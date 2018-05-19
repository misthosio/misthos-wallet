open Jest;

open PrimitiveTypes;

open Event;

open ValidationHelpers;

let () =
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
  F.withCached(
    ~scope="Any proposal type",
    "when submitting the identical proposal twice",
    () => G.withUserSessions(2),
    sessions => {
      let (user1, user2) = G.twoUserSessionsFromArray(sessions);
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withPartnerProposed(~supporter=user1, ~prospect=user2)
      );
    },
    (_sessions, log) =>
      testValidationResult(
        log |> constructState,
        L.(log |> lastItem),
        Validation.Ignore,
      ),
  );
  F.withCached(
    ~scope="Any proposal type",
    "with the wrong policy",
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
          |> withPartnerProposed(
               ~policy=Policy.unanimousMinusOne,
               ~supporter=user1,
               ~prospect=user2,
             )
          |> lastItem
        ),
        Validation.PolicyMissmatch,
      );
    },
  );
  F.withCached(
    ~scope="Any proposal type",
    "when the supporter is a non-partner",
    () => G.withUserSessions(3),
    sessions => {
      let (user1, _user2, _user3) = G.threeUserSessionsFromArray(sessions);
      L.(createVenture(user1) |> withFirstPartner(user1));
    },
    (sessions, log) => {
      let (_user1, user2, user3) = G.threeUserSessionsFromArray(sessions);
      testValidationResult(
        log |> constructState,
        L.(
          log
          |> withPartnerProposed(~supporter=user2, ~prospect=user3)
          |> lastItem
        ),
        Validation.InvalidIssuer,
      );
    },
  );
  F.withCached(
    ~scope="Any proposal type",
    "when the supporter is not the signer",
    () => G.withUserSessions(3),
    sessions => {
      let (user1, _user2, _user3) = G.threeUserSessionsFromArray(sessions);
      L.(createVenture(user1) |> withFirstPartner(user1));
    },
    (sessions, log) => {
      let (user1, user2, user3) = G.threeUserSessionsFromArray(sessions);
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
    },
  );
  F.withCached(
    ~scope="Any proposal type",
    "when the proposal was already submitted by this partner",
    () => G.withUserSessions(2),
    sessions => {
      let (user1, user2) = G.twoUserSessionsFromArray(sessions);
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withPartnerProposed(~supporter=user1, ~prospect=user2)
      );
    },
    (sessions, log) => {
      let (user1, user2) = G.twoUserSessionsFromArray(sessions);
      testValidationResult(
        log |> constructState,
        L.(
          log
          |> withPartnerProposed(~supporter=user1, ~prospect=user2)
          |> lastItem
        ),
        Validation.BadData("This proposal already exists"),
      );
    },
  );
  F.withCached(
    ~scope="Any proposal type",
    "when the same proposal was already made by another partner",
    () => G.withUserSessions(3),
    sessions => {
      let (user1, user2, user3) = G.threeUserSessionsFromArray(sessions);
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withPartner(user2, ~supporters=[user1])
        |> withPartnerProposed(~supporter=user1, ~prospect=user3)
      );
    },
    (sessions, log) => {
      let (_user1, user2, user3) = G.threeUserSessionsFromArray(sessions);
      testValidationResult(
        log |> constructState,
        L.(
          log
          |> withPartnerProposed(~supporter=user2, ~prospect=user3)
          |> lastItem
        ),
        Validation.Ok,
      );
    },
  );
});

describe("Any rejection type", () => {
  F.withCached(
    ~scope="Any rejection type",
    "when the process is unknown",
    () => G.withUserSessions(3),
    sessions => {
      let (user1, user2, user3) = G.threeUserSessionsFromArray(sessions);
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withPartner(user2, ~supporters=[user1])
        |> withPartnerProposed(~supporter=user1, ~prospect=user3)
      );
    },
    (sessions, log) => {
      let (_user1, user2, _user3) = G.threeUserSessionsFromArray(sessions);
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
    },
  );
  F.withCached(
    ~scope="Any rejection type",
    "when the rejector is not a partner",
    () => G.withUserSessions(3),
    sessions => {
      let (user1, user2, user3) = G.threeUserSessionsFromArray(sessions);
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withPartner(user2, ~supporters=[user1])
        |> withPartnerProposed(~supporter=user1, ~prospect=user3)
      );
    },
    (sessions, log) => {
      let (_user1, _user2, user3) = G.threeUserSessionsFromArray(sessions);
      let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
      testValidationResult(
        log |> constructState,
        L.(log |> withPartnerRejected(user3, proposal) |> lastItem),
        Validation.InvalidIssuer,
      );
    },
  );
  F.withCached(
    ~scope="Any rejection type",
    "when the rejector is not the signer",
    () => G.withUserSessions(3),
    sessions => {
      let (user1, user2, user3) = G.threeUserSessionsFromArray(sessions);
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withPartner(user2, ~supporters=[user1])
        |> withPartnerProposed(~supporter=user1, ~prospect=user3)
      );
    },
    (sessions, log) => {
      let (user1, user2, _user3) = G.threeUserSessionsFromArray(sessions);
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
    },
  );
  F.withCached(
    ~scope="Any rejection type",
    "when the rejection has already been submitted",
    () => G.withUserSessions(4),
    sessions => {
      let (user1, user2, user3, user4) =
        G.fourUserSessionsFromArray(sessions);
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withPartner(user2, ~supporters=[user1])
        |> withPartner(user3, ~supporters=[user1, user2])
        |> withPartnerProposed(~supporter=user1, ~prospect=user4)
      );
    },
    (sessions, log) => {
      let (_user1, user2, _user3, _user4) =
        G.fourUserSessionsFromArray(sessions);
      let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
      let log = log |> L.withPartnerRejected(user2, proposal);
      testValidationResult(
        log |> constructState,
        L.(log |> lastItem),
        Validation.Ignore,
      );
    },
  );
  F.withCached(
    ~scope="Any rejection type",
    "when the rejector has already endorsed",
    () => G.withUserSessions(4),
    sessions => {
      let (user1, user2, user3, user4) =
        G.fourUserSessionsFromArray(sessions);
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withPartner(user2, ~supporters=[user1])
        |> withPartner(user3, ~supporters=[user1, user2])
        |> withPartnerProposed(~supporter=user1, ~prospect=user4)
      );
    },
    (sessions, log) => {
      let (_user1, user2, _user3, _user4) =
        G.fourUserSessionsFromArray(sessions);
      let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
      let log = log |> L.withPartnerEndorsed(user2, proposal);
      testValidationResult(
        log |> constructState,
        L.(log |> withPartnerRejected(user2, proposal) |> lastItem),
        Validation.AlreadyEndorsed,
      );
    },
  );
  F.withCached(
    ~scope="Any rejection type",
    "when the rejection is fine",
    () => G.withUserSessions(3),
    sessions => {
      let (user1, user2, user3) = G.threeUserSessionsFromArray(sessions);
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withPartner(user2, ~supporters=[user1])
        |> withPartnerProposed(~supporter=user1, ~prospect=user3)
      );
    },
    (sessions, log) => {
      let (_user1, user2, _user3) = G.threeUserSessionsFromArray(sessions);
      let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
      testValidationResult(
        log |> constructState,
        L.(log |> withPartnerRejected(user2, proposal) |> lastItem),
        Validation.Ok,
      );
    },
  );
});

describe("Any endorsement type", () => {
  F.withCached(
    ~scope="Any endorsement type",
    "when the process is unknown",
    () => G.withUserSessions(3),
    sessions => {
      let (user1, user2, user3) = G.threeUserSessionsFromArray(sessions);
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withPartner(user2, ~supporters=[user1])
        |> withPartnerProposed(~supporter=user1, ~prospect=user3)
      );
    },
    (sessions, log) => {
      let (_user1, user2, _user3) = G.threeUserSessionsFromArray(sessions);
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
    },
  );
  F.withCached(
    ~scope="Any endorsement type",
    "when the supporter is not a partner",
    () => G.withUserSessions(3),
    sessions => {
      let (user1, user2, user3) = G.threeUserSessionsFromArray(sessions);
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withPartner(user2, ~supporters=[user1])
        |> withPartnerProposed(~supporter=user1, ~prospect=user3)
      );
    },
    (sessions, log) => {
      let (_user1, _user2, user3) = G.threeUserSessionsFromArray(sessions);
      let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
      testValidationResult(
        log |> constructState,
        L.(log |> withPartnerEndorsed(user3, proposal) |> lastItem),
        Validation.InvalidIssuer,
      );
    },
  );
  F.withCached(
    ~scope="Any endorsement type",
    "when the supporter is not the signer",
    () => G.withUserSessions(3),
    sessions => {
      let (user1, user2, user3) = G.threeUserSessionsFromArray(sessions);
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withPartner(user2, ~supporters=[user1])
        |> withPartnerProposed(~supporter=user1, ~prospect=user3)
      );
    },
    (sessions, log) => {
      let (user1, user2, _user3) = G.threeUserSessionsFromArray(sessions);
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
    },
  );
  F.withCached(
    ~scope="Any endorsement type",
    "when the endorsement has already been submitted",
    () => G.withUserSessions(4),
    sessions => {
      let (user1, user2, user3, user4) =
        G.fourUserSessionsFromArray(sessions);
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withPartner(user2, ~supporters=[user1])
        |> withPartner(user3, ~supporters=[user1, user2])
        |> withPartnerProposed(~supporter=user1, ~prospect=user4)
      );
    },
    (sessions, log) => {
      let (_user1, user2, _user3, _user4) =
        G.fourUserSessionsFromArray(sessions);
      let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
      let log = log |> L.withPartnerEndorsed(user2, proposal);
      testValidationResult(
        log |> constructState,
        L.(log |> lastItem),
        Validation.Ignore,
      );
    },
  );
  F.withCached(
    ~scope="Any endorsement type",
    "when the endorsement is fine",
    () => G.withUserSessions(3),
    sessions => {
      let (user1, user2, user3) = G.threeUserSessionsFromArray(sessions);
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withPartner(user2, ~supporters=[user1])
        |> withPartnerProposed(~supporter=user1, ~prospect=user3)
      );
    },
    (sessions, log) => {
      let (_user1, user2, _user3) = G.threeUserSessionsFromArray(sessions);
      let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
      testValidationResult(
        log |> constructState,
        L.(log |> withPartnerEndorsed(user2, proposal) |> lastItem),
        Validation.Ok,
      );
    },
  );
  describe("Any acceptance type", () => {
    F.withCached(
      ~scope="Any acceptance type",
      "when everything is fine",
      () => G.withUserSessions(3),
      sessions => {
        let (user1, user2, user3) = G.threeUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartnerProposed(~supporter=user1, ~prospect=user3)
        );
      },
      (sessions, log) => {
        let (_user1, user2, _user3) = G.threeUserSessionsFromArray(sessions);
        let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
        let log = log |> L.withPartnerEndorsed(user2, proposal);
        testValidationResult(
          log |> constructState,
          L.(log |> withPartnerAccepted(proposal) |> lastItem),
          Validation.Ok,
        );
      },
    );
    F.withCached(
      ~scope="Any acceptance type",
      "New partners don't effect eligiblity",
      () => G.withUserSessions(4),
      sessions => {
        let (user1, user2, user3, _user4) =
          G.fourUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartnerProposed(~supporter=user1, ~prospect=user3)
        );
      },
      (sessions, log) => {
        let (user1, user2, _user3, user4) =
          G.fourUserSessionsFromArray(sessions);
        let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
        let log =
          L.(
            log
            |> L.withPartner(user4, ~supporters=[user1, user2])
            |> withPartnerEndorsed(user2, proposal)
          );
        testValidationResult(
          log |> constructState,
          L.(log |> withPartnerAccepted(proposal) |> lastItem),
          Validation.Ok,
        );
      },
    );
    F.withCached(
      ~scope="Any acceptance type",
      "when the data is wrong",
      () => G.withUserSessions(3),
      sessions => {
        let (user1, user2, user3) = G.threeUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartnerProposed(~supporter=user1, ~prospect=user3)
        );
      },
      (sessions, log) => {
        let (_user1, user2, _user3) = G.threeUserSessionsFromArray(sessions);
        let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
        let log = log |> L.withPartnerEndorsed(user2, proposal);
        testValidationResult(
          log |> constructState,
          L.(
            log
            |> appendSystemEvent(
                 PartnerAccepted({
                   ...Partner.Accepted.fromProposal(proposal),
                   data: {
                     ...proposal.data,
                     id: UserId.fromString("bad"),
                   },
                 }),
               )
            |> lastItem
          ),
          Validation.BadData("Data doesn't match proposal"),
        );
      },
    );
    F.withCached(
      ~scope="Any acceptance type",
      "when the policy is not fullfilled",
      () => G.withUserSessions(3),
      sessions => {
        let (user1, user2, user3) = G.threeUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartnerProposed(~supporter=user1, ~prospect=user3)
        );
      },
      (_sessions, log) => {
        let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
        testValidationResult(
          log |> constructState,
          L.(log |> withPartnerAccepted(proposal) |> lastItem),
          Validation.PolicyNotFulfilled,
        );
      },
    );
  });
});
