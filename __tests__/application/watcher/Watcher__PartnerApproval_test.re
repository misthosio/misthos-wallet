open Jest;

module PartnerApproval = Watcher__PartnerApproval;

open WatcherHelpers;

let () =
  describe("Watcher__PartnerApproval", () => {
    describe("Will approve the creator", () => {
      let (user1, _user2) = G.twoUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withPartnerProposed(~proposer=user1, ~prospect=user1)
        );
      let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
      let log = log |> L.withPartnerEndorsed(user1, proposal);
      let watcher = PartnerApproval.make(proposal, log |> L.eventLog);
      testWatcherHasEventPending(
        "PartnerAccepted",
        watcher,
        log |> L.systemIssuer,
        fun
        | PartnerAccepted({data}) => data == proposal.data
        | _ => false,
      );
    });
    describe("With 1 partner and a proposal", () => {
      let (user1, user2) = G.twoUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartnerProposed(~proposer=user1, ~prospect=user2)
        );
      let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
      let log = log |> L.withPartnerEndorsed(user1, proposal);
      let watcher = PartnerApproval.make(proposal, log |> L.eventLog);
      testWatcherHasEventPending(
        "PartnerAccepted",
        watcher,
        log |> L.systemIssuer,
        fun
        | PartnerAccepted({data}) => data == proposal.data
        | _ => false,
      );
    });
    describe("Completes when the partner is accepted", () => {
      let (user1, user2) = G.twoUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartnerProposed(~proposer=user1, ~prospect=user2)
        );
      let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
      let log = log |> L.withPartnerEndorsed(user1, proposal);
      let watcher = PartnerApproval.make(proposal, log |> L.eventLog);
      let log = log |> L.withPartnerAccepted(proposal);
      watcher#receive(log |> L.lastItem);
      testWatcherHasCompleted(watcher);
    });
    describe("With 2 users and a proposal", () => {
      let (user1, user2, user3) = G.threeUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartnerProposed(~proposer=user1, ~prospect=user3)
        );
      let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
      let log = log |> L.withPartnerEndorsed(user1, proposal);
      let watcher = PartnerApproval.make(proposal, log |> L.eventLog);
      testWatcherHasNoEventPending(watcher);
    });
    describe("With 2 users and a proposal and endorsement", () => {
      let (user1, user2, user3) = G.threeUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartnerProposed(~proposer=user1, ~prospect=user3)
        );
      let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
      let log =
        L.(
          log
          |> withPartnerEndorsed(user1, proposal)
          |> withPartnerEndorsed(user2, proposal)
        );
      let watcher = PartnerApproval.make(proposal, log |> L.eventLog);
      testWatcherHasEventPending(
        "PartnerAccepted",
        watcher,
        log |> L.systemIssuer,
        fun
        | PartnerAccepted({data}) => data == proposal.data
        | _ => false,
      );
    });
    describe("With 2 users and a removal and a proposal", () => {
      let (user1, user2, user3) = G.threeUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartnerRemoved(user2, ~supporters=[user1])
          |> withPartnerProposed(~proposer=user1, ~prospect=user3)
        );
      let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
      let log = log |> L.withPartnerEndorsed(user1, proposal);
      let watcher = PartnerApproval.make(proposal, log |> L.eventLog);
      testWatcherHasEventPending(
        "PartnerAccepted",
        watcher,
        log |> L.systemIssuer,
        fun
        | PartnerAccepted({data}) => data == proposal.data
        | _ => false,
      );
    });
    describe("Process gets denied when it has been rejected", () => {
      let (user1, user2, user3) = G.threeUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartnerProposed(~proposer=user1, ~prospect=user3)
        );
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
    });
    describe("Completes when the partner is denied", () => {
      let (user1, user2, user3) = G.threeUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartnerProposed(~proposer=user1, ~prospect=user3)
        );
      let proposal = log |> L.lastEvent |> Event.getPartnerProposedExn;
      let log = log |> L.withPartnerRejected(user2, proposal);
      let watcher = PartnerApproval.make(proposal, log |> L.eventLog);
      let log = log |> L.withPartnerDenied(proposal);
      watcher#receive(log |> L.lastItem);
      testWatcherHasCompleted(watcher);
    });
  });
