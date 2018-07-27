open Jest;

module CustodianApproval = Watcher__CustodianApproval;

open WatcherHelpers;

let () =
  describe("Watcher__CustodianApproval", () => {
    describe("With 1 partner and a proposal", () => {
      let (user1, _user2) = G.twoUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withCustodianProposed(~proposer=user1, ~custodian=user1)
        );
      let proposal = log |> L.lastEvent |> Event.getCustodianProposedExn;
      let log = log |> L.withCustodianEndorsed(user1, proposal);
      let watcher = CustodianApproval.make(proposal, log |> L.eventLog);
      testWatcherHasEventPending(
        "CustodianAccepted",
        watcher,
        log |> L.systemIssuer,
        fun
        | CustodianAccepted({data}) => data == proposal.data
        | _ => false,
      );
    });
    describe("Completes when the custodian is accepted", () => {
      let (user1, _user2) = G.twoUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withCustodianProposed(~proposer=user1, ~custodian=user1)
        );
      let proposal = log |> L.lastEvent |> Event.getCustodianProposedExn;
      let log = log |> L.withCustodianEndorsed(user1, proposal);
      let watcher = CustodianApproval.make(proposal, log |> L.eventLog);
      let log = log |> L.withCustodianAccepted(proposal);
      watcher#receive(log |> L.lastItem);
      testWatcherHasCompleted(watcher);
    });
    describe("Completes when the partner is removed", () => {
      let (user1, user2) = G.twoUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withCustodianProposed(~proposer=user1, ~custodian=user2)
        );
      let proposal = log |> L.lastEvent |> Event.getCustodianProposedExn;
      let log =
        L.(
          log
          |> withCustodianEndorsed(user1, proposal)
          |> withPartnerRemoved(user2, ~supporters=[user1])
        );
      let watcher = CustodianApproval.make(proposal, log |> L.eventLog);
      testWatcherHasCompleted(watcher);
    });
    describe("Only completes if the partner process has completed", () => {
      let (user1, user2, user3) = G.threeUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartnerProposed(~proposer=user1, ~prospect=user3)
          |> withCustodianProposed(~proposer=user1, ~custodian=user3)
        );
      let proposal = log |> L.lastEvent |> Event.getCustodianProposedExn;
      let log =
        L.(
          log
          |> withCustodianEndorsed(user1, proposal)
          |> withPartnerRemoved(user2, ~supporters=[user1])
        );
      let watcher = CustodianApproval.make(proposal, log |> L.eventLog);
      testWatcherHasNoEventPending(watcher);
    });
    describe("With 2 users and a proposal", () => {
      let (user1, user2) = G.twoUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withCustodianProposed(~proposer=user1, ~custodian=user2)
        );
      let proposal = log |> L.lastEvent |> Event.getCustodianProposedExn;
      let watcher = CustodianApproval.make(proposal, log |> L.eventLog);
      testWatcherHasNoEventPending(watcher);
    });
    describe("With 2 users and a proposal and 2 endorsements", () => {
      let (user1, user2) = G.twoUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withCustodianProposed(~proposer=user1, ~custodian=user2)
        );
      let proposal = log |> L.lastEvent |> Event.getCustodianProposedExn;
      let log =
        L.(
          log
          |> withCustodianEndorsed(user1, proposal)
          |> withCustodianEndorsed(user2, proposal)
        );
      let watcher = CustodianApproval.make(proposal, log |> L.eventLog);
      testWatcherHasEventPending(
        "CustodianAccepted",
        watcher,
        log |> L.systemIssuer,
        fun
        | CustodianAccepted({data}) => data == proposal.data
        | _ => false,
      );
    });
    describe("With 2 users and a removal and a proposal", () => {
      let (user1, user2) = G.twoUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartnerRemoved(user2, ~supporters=[user1])
          |> withCustodianProposed(~proposer=user1, ~custodian=user1)
        );
      let proposal = log |> L.lastEvent |> Event.getCustodianProposedExn;
      let log = log |> L.withCustodianEndorsed(user1, proposal);
      let watcher = CustodianApproval.make(proposal, log |> L.eventLog);
      testWatcherHasEventPending(
        "CustodianAccepted",
        watcher,
        log |> L.systemIssuer,
        fun
        | CustodianAccepted({data}) => data == proposal.data
        | _ => false,
      );
    });
    describe("With 2 users and a proposal and a removal", () => {
      let (user1, user2) = G.twoUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withCustodianProposed(~proposer=user1, ~custodian=user1)
        );
      let proposal = log |> L.lastEvent |> Event.getCustodianProposedExn;
      let log = log |> L.withCustodianEndorsed(user1, proposal);
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
    });
    describe("Process gets denied when it has been rejected", () => {
      let (user1, user2, user3) = G.threeUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartner(user3, ~supporters=[user1, user2])
          |> withCustodianProposed(~proposer=user1, ~custodian=user3)
        );
      let proposal = log |> L.lastEvent |> Event.getCustodianProposedExn;
      let log = log |> L.withCustodianRejected(user2, proposal);
      let watcher = CustodianApproval.make(proposal, log |> L.eventLog);
      testWatcherHasEventPending(
        "CustodianDenied",
        watcher,
        log |> L.systemIssuer,
        fun
        | CustodianDenied(_) => true
        | _ => false,
      );
    });
    describe("Completes when the custodian is denied", () => {
      let (user1, user2, user3) = G.threeUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartner(user3, ~supporters=[user1, user2])
          |> withCustodianProposed(~proposer=user1, ~custodian=user3)
        );
      let proposal = log |> L.lastEvent |> Event.getCustodianProposedExn;
      let log = log |> L.withCustodianRejected(user2, proposal);
      let watcher = CustodianApproval.make(proposal, log |> L.eventLog);
      let log = log |> L.withCustodianDenied(proposal);
      watcher#receive(log |> L.lastItem);
      testWatcherHasCompleted(watcher);
    });
  });
