open PrimitiveTypes;

open Jest;

module KeyChain = CustodianKeyChain;

module CustodianKeyChain = Watcher__CustodianKeyChain;

open WatcherHelpers;

let keyChainEq = (keyChainA, keyChainB) =>
  keyChainA |> KeyChain.encode == (keyChainB |> KeyChain.encode);

let () =
  describe("Watcher__CustodianKeyChain", () => {
    F.withCached(
      ~scope="Watcher__CustodianKeyChain",
      "Will create the initial keychain",
      () => G.withUserSessions(2),
      sessions => {
        let (user1, _user2) = G.twoUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
          |> withCustodian(user1, ~supporters=[user1])
        );
      },
      (sessions, log) => {
        let (user1, _user2) = G.twoUserSessionsFromArray(sessions);
        let acceptance = log |> L.lastEvent |> Event.getCustodianAcceptedExn;
        let watcher =
          CustodianKeyChain.make(user1, acceptance, log |> L.eventLog);
        testWatcherHasEventPending(
          "CustodianKeyChainUpdated",
          watcher,
          user1.issuerKeyPair,
          fun
          | CustodianKeyChainUpdated({
              custodianApprovalProcess,
              custodianId,
              keyChain,
            })
              when
                ProcessId.eq(custodianApprovalProcess, acceptance.processId)
                && UserId.eq(custodianId, user1.userId) =>
            keyChain
            |> keyChainEq(
                 G.custodianKeyChain(
                   ~ventureId=log |> L.ventureId,
                   ~keyChainIdx=0,
                   user1,
                 ),
               )
          | _ => false,
        );
      },
    );
    F.withCached(
      ~scope="Watcher__CustodianKeyChain",
      "Is idle when the keychain has been updated",
      () => G.withUserSessions(2),
      sessions => {
        let (user1, _user2) = G.twoUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
          |> withCustodian(user1, ~supporters=[user1])
        );
      },
      (sessions, log) => {
        let (user1, _user2) = G.twoUserSessionsFromArray(sessions);
        let acceptance = log |> L.lastEvent |> Event.getCustodianAcceptedExn;
        let watcher =
          CustodianKeyChain.make(user1, acceptance, log |> L.eventLog);
        let log = log |> L.withCustodianKeyChain(user1);
        watcher#receive(log |> L.lastItem);
        testWatcherHasNoEventPending(watcher);
      },
    );
    F.withCached(
      ~scope="Watcher__CustodianKeyChain",
      "Will update the keychain when a partner is removed",
      () => G.withUserSessions(2),
      sessions => {
        let (user1, _user2) = G.twoUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
          |> withCustodian(user1, ~supporters=[user1])
        );
      },
      (sessions, log) => {
        let (user1, user2) = G.twoUserSessionsFromArray(sessions);
        let acceptance = log |> L.lastEvent |> Event.getCustodianAcceptedExn;
        let log =
          L.(
            log
            |> withCustodianKeyChain(user1)
            |> withPartner(user2, ~supporters=[user1])
            |> withPartnerRemoved(user2, ~supporters=[user1])
          );
        let watcher =
          CustodianKeyChain.make(user1, acceptance, log |> L.eventLog);
        testWatcherHasEventPending(
          "CustodianKeyChainUpdated",
          watcher,
          user1.issuerKeyPair,
          fun
          | CustodianKeyChainUpdated({
              custodianApprovalProcess,
              custodianId,
              keyChain,
            })
              when
                ProcessId.eq(custodianApprovalProcess, acceptance.processId)
                && UserId.eq(custodianId, user1.userId) =>
            keyChain
            |> keyChainEq(
                 G.custodianKeyChain(
                   ~ventureId=log |> L.ventureId,
                   ~keyChainIdx=1,
                   user1,
                 ),
               )
          | _ => false,
        );
      },
    );
    F.withCached(
      ~scope="Watcher__CustodianKeyChain",
      "Keeps increasing the index accross multiple removals",
      () => G.withUserSessions(2),
      sessions => {
        let (user1, user2) = G.twoUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
          |> withCustodian(user1, ~supporters=[user1])
          |> withCustodianKeyChain(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withPartnerRemoved(user1, ~supporters=[user2])
          |> withPartner(user1, ~supporters=[user2])
          |> withCustodian(user1, ~supporters=[user2, user1])
        );
      },
      (sessions, log) => {
        let (user1, _user2) = G.twoUserSessionsFromArray(sessions);
        let acceptance = log |> L.lastEvent |> Event.getCustodianAcceptedExn;
        let watcher =
          CustodianKeyChain.make(user1, acceptance, log |> L.eventLog);
        testWatcherHasEventPending(
          "CustodianKeyChainUpdated",
          watcher,
          user1.issuerKeyPair,
          fun
          | CustodianKeyChainUpdated({
              custodianApprovalProcess,
              custodianId,
              keyChain,
            })
              when
                ProcessId.eq(custodianApprovalProcess, acceptance.processId)
                && UserId.eq(custodianId, user1.userId) =>
            keyChain
            |> keyChainEq(
                 G.custodianKeyChain(
                   ~ventureId=log |> L.ventureId,
                   ~keyChainIdx=1,
                   user1,
                 ),
               )
          | _ => false,
        );
      },
    );
    describe("Completion", () => {
      F.withCached(
        ~scope="Watcher__CustodianKeyChain",
        "when the custodian is a different user",
        () => G.withUserSessions(2),
        sessions => {
          let (user1, user2) = G.twoUserSessionsFromArray(sessions);
          L.(
            createVenture(user1)
            |> withFirstPartner(user1)
            |> withAccount(~supporter=user1)
            |> withCustodian(user1, ~supporters=[user1])
            |> withPartner(user2, ~supporters=[user1])
            |> withCustodian(user2, ~supporters=[user1, user2])
          );
        },
        (sessions, log) => {
          let (user1, user2) = G.twoUserSessionsFromArray(sessions);
          let acceptance = log |> L.lastEvent |> Event.getCustodianAcceptedExn;
          let log =
            L.(log |> withCustodianRemoved(user2, ~supporters=[user1]));
          let watcher =
            CustodianKeyChain.make(user1, acceptance, log |> L.eventLog);
          testWatcherHasCompleted(watcher);
        },
      );
      F.withCached(
        ~scope="Watcher__CustodianKeyChain",
        "when the custodian is removed",
        () => G.withUserSessions(2),
        sessions => {
          let (user1, user2) = G.twoUserSessionsFromArray(sessions);
          L.(
            createVenture(user1)
            |> withFirstPartner(user1)
            |> withAccount(~supporter=user1)
            |> withCustodian(user1, ~supporters=[user1])
            |> withPartner(user2, ~supporters=[user1])
            |> withCustodian(user2, ~supporters=[user1, user2])
          );
        },
        (sessions, log) => {
          let (user1, user2) = G.twoUserSessionsFromArray(sessions);
          let acceptance = log |> L.lastEvent |> Event.getCustodianAcceptedExn;
          let log =
            L.(log |> withCustodianRemoved(user2, ~supporters=[user1]));
          let watcher =
            CustodianKeyChain.make(user2, acceptance, log |> L.eventLog);
          testWatcherHasCompleted(watcher);
        },
      );
      F.withCached(
        ~scope="Watcher__CustodianKeyChain",
        "when the partner is removed",
        () => G.withUserSessions(2),
        sessions => {
          let (user1, user2) = G.twoUserSessionsFromArray(sessions);
          L.(
            createVenture(user1)
            |> withFirstPartner(user1)
            |> withAccount(~supporter=user1)
            |> withCustodian(user1, ~supporters=[user1])
            |> withPartner(user2, ~supporters=[user1])
            |> withCustodian(user2, ~supporters=[user1, user2])
          );
        },
        (sessions, log) => {
          let (user1, user2) = G.twoUserSessionsFromArray(sessions);
          let acceptance = log |> L.lastEvent |> Event.getCustodianAcceptedExn;
          let log =
            L.(log |> withPartnerRemoved(user2, ~supporters=[user1]));
          let watcher =
            CustodianKeyChain.make(user2, acceptance, log |> L.eventLog);
          testWatcherHasCompleted(watcher);
        },
      );
    });
  });
