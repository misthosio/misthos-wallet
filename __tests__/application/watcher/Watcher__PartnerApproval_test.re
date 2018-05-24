open Jest;

module PartnerApproval = Watcher__PartnerApproval;

open WatcherHelpers;

let () =
  describe("Watcher__PartnerApproval", () => {
    F.withCached(
      ~scope="Watcher__PartnerApproval",
      "Will approve the creator",
      () => G.withUserSessions(2),
      sessions => {
        let (user1, _user2) = G.twoUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withPartnerProposed(~proposer=user1, ~prospect=user1)
        );
      },
      (_sessions, log) => {
        let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
        let watcher = PartnerApproval.make(proposal, log |> L.eventLog);
        testWatcherHasEventPending(
          "PartnerAccepted",
          watcher,
          log |> L.systemIssuer,
          fun
          | PartnerAccepted({data}) => data == proposal.data
          | _ => false,
        );
      },
    );
    F.withCached(
      ~scope="Watcher__PartnerApproval",
      "With 1 partner and a proposal",
      () => G.withUserSessions(2),
      sessions => {
        let (user1, user2) = G.twoUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartnerProposed(~proposer=user1, ~prospect=user2)
        );
      },
      (_sessions, log) => {
        let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
        let watcher = PartnerApproval.make(proposal, log |> L.eventLog);
        testWatcherHasEventPending(
          "PartnerAccepted",
          watcher,
          log |> L.systemIssuer,
          fun
          | PartnerAccepted({data}) => data == proposal.data
          | _ => false,
        );
      },
    );
    F.withCached(
      ~scope="Watcher__PartnerApproval",
      "Completes when the partner is accepted",
      () => G.withUserSessions(2),
      sessions => {
        let (user1, user2) = G.twoUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartnerProposed(~proposer=user1, ~prospect=user2)
        );
      },
      (_sessions, log) => {
        let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
        let watcher = PartnerApproval.make(proposal, log |> L.eventLog);
        let log = log |> L.withPartnerAccepted(proposal);
        watcher#receive(log |> L.lastItem);
        testWatcherHasCompleted(watcher);
      },
    );
    F.withCached(
      ~scope="Watcher__PartnerApproval",
      "With 2 users and a proposal",
      () => G.withUserSessions(3),
      sessions => {
        let (user1, user2, user3) = G.threeUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartnerProposed(~proposer=user1, ~prospect=user3)
        );
      },
      (_sessions, log) => {
        let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
        let watcher = PartnerApproval.make(proposal, log |> L.eventLog);
        testWatcherHasNoEventPending(watcher);
      },
    );
    F.withCached(
      ~scope="Watcher__PartnerApproval",
      "With 2 users and a proposal and endorsement",
      () => G.withUserSessions(3),
      sessions => {
        let (user1, user2, user3) = G.threeUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartnerProposed(~proposer=user1, ~prospect=user3)
        );
      },
      (sessions, log) => {
        let (_user1, user2, _user3) = G.threeUserSessionsFromArray(sessions);
        let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
        let log = log |> L.withPartnerEndorsed(user2, proposal);
        let watcher = PartnerApproval.make(proposal, log |> L.eventLog);
        testWatcherHasEventPending(
          "PartnerAccepted",
          watcher,
          log |> L.systemIssuer,
          fun
          | PartnerAccepted({data}) => data == proposal.data
          | _ => false,
        );
      },
    );
    F.withCached(
      ~scope="Watcher__PartnerApproval",
      "With 2 users and a removal and a proposal",
      () => G.withUserSessions(3),
      sessions => {
        let (user1, user2, user3) = G.threeUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartnerRemoved(user2, ~supporters=[user1])
          |> withPartnerProposed(~proposer=user1, ~prospect=user3)
        );
      },
      (_sessions, log) => {
        let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
        let watcher = PartnerApproval.make(proposal, log |> L.eventLog);
        testWatcherHasEventPending(
          "PartnerAccepted",
          watcher,
          log |> L.systemIssuer,
          fun
          | PartnerAccepted({data}) => data == proposal.data
          | _ => false,
        );
      },
    );
    F.withCached(
      ~scope="Watcher__PartnerApproval",
      "Process gets denied when it has been rejected",
      () => G.withUserSessions(3),
      sessions => {
        let (user1, user2, user3) = G.threeUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartnerProposed(~proposer=user1, ~prospect=user3)
        );
      },
      (sessions, log) => {
        let (_user1, user2, _user3) = G.threeUserSessionsFromArray(sessions);
        let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
        let log = log |> L.withPartnerRejected(user2, proposal);
        let watcher = PartnerApproval.make(proposal, log |> L.eventLog);
        testWatcherHasEventPending(
          "PartnerDenied",
          watcher,
          log |> L.systemIssuer,
          fun
          | PartnerDenied(_) => true
          | _ => false,
        );
      },
    );
    F.withCached(
      ~scope="Watcher__PartnerApproval",
      "Completes when the partner is denied",
      () => G.withUserSessions(3),
      sessions => {
        let (user1, user2, user3) = G.threeUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartnerProposed(~proposer=user1, ~prospect=user3)
        );
      },
      (sessions, log) => {
        let (_user1, user2, _user3) = G.threeUserSessionsFromArray(sessions);
        let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
        let log = log |> L.withPartnerRejected(user2, proposal);
        let watcher = PartnerApproval.make(proposal, log |> L.eventLog);
        let log = log |> L.withPartnerDenied(proposal);
        watcher#receive(log |> L.lastItem);
        testWatcherHasCompleted(watcher);
      },
    );
  });
