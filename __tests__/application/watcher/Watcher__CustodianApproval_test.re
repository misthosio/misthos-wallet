open PrimitiveTypes;

open Jest;

module CustodianApproval = Watcher__CustodianApproval;

open WatcherHelpers;

let () = {
  describe("With 1 partner and a proposal", () => {
    let user1 = G.userSession("user1" |> UserId.fromString);
    let log =
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withCustodianProposed(~supporter=user1, ~custodian=user1)
      );
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
  });
  describe("Completes when the custodian is accepted", () => {
    let user1 = G.userSession("user1" |> UserId.fromString);
    let log =
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withCustodianProposed(~supporter=user1, ~custodian=user1)
      );
    let proposal = log |> L.lastEvent |> Event.getCustodianProposedExn;
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
        |> withCustodianProposed(~supporter=user1, ~custodian=user2)
      );
    let proposal = log |> L.lastEvent |> Event.getCustodianProposedExn;
    let log = log |> L.withPartnerRemoved(user2, ~supporters=[user1]);
    let watcher = CustodianApproval.make(proposal, log |> L.eventLog);
    testWatcherHasCompleted(watcher);
  });
  describe("With 2 users and a proposal", () => {
    let (user1, user2) = G.twoUserSessions();
    let log =
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withPartner(user2, ~supporters=[user1])
        |> withCustodianProposed(~supporter=user1, ~custodian=user2)
      );
    let proposal = log |> L.lastEvent |> Event.getCustodianProposedExn;
    let watcher = CustodianApproval.make(proposal, log |> L.eventLog);
    testWatcherHasNoEventPending(watcher);
  });
  describe("With 2 users and a proposal and endorsement", () => {
    let (user1, user2) = G.twoUserSessions();
    let log =
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withPartner(user2, ~supporters=[user1])
        |> withCustodianProposed(~supporter=user1, ~custodian=user2)
      );
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
  });
  describe("With 2 users and a removal and a proposal", () => {
    let (user1, user2) = G.twoUserSessions();
    let log =
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withPartner(user2, ~supporters=[user1])
        |> withPartnerRemoved(user2, ~supporters=[user1])
        |> withCustodianProposed(~supporter=user1, ~custodian=user1)
      );
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
  });
  describe("With 2 users and a proposal and a removal", () => {
    let (user1, user2) = G.twoUserSessions();
    let log =
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withPartner(user2, ~supporters=[user1])
        |> withCustodianProposed(~supporter=user1, ~custodian=user1)
      );
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
  });
};
