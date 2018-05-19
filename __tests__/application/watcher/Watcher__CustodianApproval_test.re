open Jest;

module CustodianApproval = Watcher__CustodianApproval;

open WatcherHelpers;

let () =
  describe("Watcher__CustodianApproval", () => {
    F.withCached(
      ~scope="Watcher__CustodianApproval",
      "With 1 partner and a proposal",
      () => G.withUserSessions(2),
      sessions => {
        let (user1, _user2) = G.twoUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withCustodianProposed(~supporter=user1, ~custodian=user1)
        );
      },
      (_sessions, log) => {
        let proposal = log |> L.lastEvent |> Event.getCustodianProposedExn;
        let watcher = CustodianApproval.make(proposal, log |> L.eventLog);
        testWatcherHasEventPending(
          "CustodianAccepted",
          watcher,
          log |> L.systemIssuer,
          fun
          | CustodianAccepted({data}) => data == proposal.data
          | _ => false,
        );
      },
    );
    F.withCached(
      ~scope="Watcher__CustodianApproval",
      "Completes when the custodian is accepted",
      () => G.withUserSessions(2),
      sessions => {
        let (user1, _user2) = G.twoUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withCustodianProposed(~supporter=user1, ~custodian=user1)
        );
      },
      (_sessions, log) => {
        let proposal = log |> L.lastEvent |> Event.getCustodianProposedExn;
        let watcher = CustodianApproval.make(proposal, log |> L.eventLog);
        let log = log |> L.withCustodianAccepted(proposal);
        watcher#receive(log |> L.lastItem);
        testWatcherHasCompleted(watcher);
      },
    );
    F.withCached(
      ~scope="Watcher__CustodianApproval",
      "Completes when the partner is removed",
      () => G.withUserSessions(2),
      sessions => {
        let (user1, user2) = G.twoUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withCustodianProposed(~supporter=user1, ~custodian=user2)
        );
      },
      (sessions, log) => {
        let (user1, user2) = G.twoUserSessionsFromArray(sessions);
        let proposal = log |> L.lastEvent |> Event.getCustodianProposedExn;
        let log = log |> L.withPartnerRemoved(user2, ~supporters=[user1]);
        let watcher = CustodianApproval.make(proposal, log |> L.eventLog);
        testWatcherHasCompleted(watcher);
      },
    );
    F.withCached(
      ~scope="Watcher__CustodianApproval",
      "With 2 users and a proposal",
      () => G.withUserSessions(2),
      sessions => {
        let (user1, user2) = G.twoUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withCustodianProposed(~supporter=user1, ~custodian=user2)
        );
      },
      (_sessions, log) => {
        let proposal = log |> L.lastEvent |> Event.getCustodianProposedExn;
        let watcher = CustodianApproval.make(proposal, log |> L.eventLog);
        testWatcherHasNoEventPending(watcher);
      },
    );
    F.withCached(
      ~scope="Watcher__CustodianApproval",
      "With 2 users and a proposal and endorsement",
      () => G.withUserSessions(2),
      sessions => {
        let (user1, user2) = G.twoUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withCustodianProposed(~supporter=user1, ~custodian=user2)
        );
      },
      (sessions, log) => {
        let (_user1, user2) = G.twoUserSessionsFromArray(sessions);
        let proposal = log |> L.lastEvent |> Event.getCustodianProposedExn;
        let log = log |> L.withCustodianEndorsed(user2, proposal);
        let watcher = CustodianApproval.make(proposal, log |> L.eventLog);
        testWatcherHasEventPending(
          "CustodianAccepted",
          watcher,
          log |> L.systemIssuer,
          fun
          | CustodianAccepted({data}) => data == proposal.data
          | _ => false,
        );
      },
    );
    F.withCached(
      ~scope="Watcher__CustodianApproval",
      "With 2 users and a removal and a proposal",
      () => G.withUserSessions(2),
      sessions => {
        let (user1, user2) = G.twoUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartnerRemoved(user2, ~supporters=[user1])
          |> withCustodianProposed(~supporter=user1, ~custodian=user1)
        );
      },
      (_sessions, log) => {
        let proposal = log |> L.lastEvent |> Event.getCustodianProposedExn;
        let watcher = CustodianApproval.make(proposal, log |> L.eventLog);
        testWatcherHasEventPending(
          "CustodianAccepted",
          watcher,
          log |> L.systemIssuer,
          fun
          | CustodianAccepted({data}) => data == proposal.data
          | _ => false,
        );
      },
    );
    F.withCached(
      ~scope="Watcher__CustodianApproval",
      "With 2 users and a proposal and a removal",
      () => G.withUserSessions(2),
      sessions => {
        let (user1, user2) = G.twoUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withCustodianProposed(~supporter=user1, ~custodian=user1)
        );
      },
      (sessions, log) => {
        let (user1, user2) = G.twoUserSessionsFromArray(sessions);
        let proposal = log |> L.lastEvent |> Event.getCustodianProposedExn;
        let watcher = CustodianApproval.make(proposal, log |> L.eventLog);
        let log = log |> L.withPartnerRemoved(user2, ~supporters=[user1]);
        watcher#receive(log |> L.lastItem);
        testWatcherHasEventPending(
          "CustodianAccepted",
          watcher,
          log |> L.systemIssuer,
          fun
          | CustodianAccepted({data}) => data == proposal.data
          | _ => false,
        );
      },
    );
  });
