open Jest;

module KeyChain = CustodianKeyChain;

module AccountKeyChain = Watcher__AccountKeyChain;

open WalletTypes;

open WatcherHelpers;

let keyChainEq = (keyChainA, keyChainB) =>
  keyChainA |> KeyChain.encode == (keyChainB |> KeyChain.encode);

let () =
  describe("Watcher__AccountKeyChain", () => {
    describe("Identifies a key chain when a custodian key chain changes", () => {
      let (user1, _user2, _user3) = F.threeUserSessions;
      let log =
        L.(
          Fixtures.createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
        );
      let acceptance =
        log |> L.lastEvent |> Event.getAccountCreationAcceptedExn;
      let log =
        L.(
          log
          |> withCustodian(user1, ~supporters=[user1])
          |> withCustodianKeyChain(user1)
        );
      let watcher =
        AccountKeyChain.make(user1, acceptance, log |> L.eventLog);
      testWatcherHasEventPending(
        "AccountKeyChainIdentified",
        watcher,
        log |> L.systemIssuer,
        fun
        | AccountKeyChainIdentified({keyChain: {identifier, accountIdx}})
            when AccountIndex.eq(accountIdx, AccountIndex.default) =>
          identifier
          == "8974ad69910afdca42d4c7c08c094c8d2a9d454d0f02b5b101eb7abd30a06d30"
        | _ => false,
      );
    });
    describe("Identifies a key chain when a partner is removed", () => {
      let (user1, user2, _user3) = F.threeUserSessions;
      let log =
        L.(
          Fixtures.createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
        );
      let acceptance =
        log |> L.lastEvent |> Event.getAccountCreationAcceptedExn;
      let log =
        L.(
          log
          |> withCustodian(user1, ~supporters=[user1])
          |> withCustodianKeyChain(user1)
          |> withAccountKeyChainIdentified
          |> withAccountKeyChainActivated(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withCustodian(user2, ~supporters=[user1, user2])
          |> withCustodianKeyChain(user2)
          |> withAccountKeyChainIdentified
          |> withAccountKeyChainActivated(user1)
          |> withCustodianRemoved(user2, ~supporters=[user1])
          |> withPartnerRemoved(user2, ~supporters=[user1])
          |> withCustodianKeyChain(~keyChainIdx=1, user1)
        );
      let watcher =
        AccountKeyChain.make(user1, acceptance, log |> L.eventLog);
      testWatcherHasEventPending(
        "AccountKeyChainIdentified",
        watcher,
        log |> L.systemIssuer,
        fun
        | AccountKeyChainIdentified({
            keyChain: {identifier, accountIdx, custodianKeyChains},
          })
            when AccountIndex.eq(accountIdx, AccountIndex.default) =>
          identifier
          == "84ba3d5f75b50fc70cdebf3b637c31c81d820519a68fd997025296ba765f2dc5"
          && custodianKeyChains
          |> List.length == 1
        | _ => false,
      );
    });
    describe("Activates a key chain when a custodian key chain changes", () => {
      let (user1, _user2, _user3) = F.threeUserSessions;
      let log =
        L.(
          Fixtures.createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
        );
      let acceptance =
        log |> L.lastEvent |> Event.getAccountCreationAcceptedExn;
      let log =
        L.(
          log
          |> withCustodian(user1, ~supporters=[user1])
          |> withCustodianKeyChain(user1)
          |> withAccountKeyChainIdentified
        );
      let watcher =
        AccountKeyChain.make(user1, acceptance, log |> L.eventLog);
      testWatcherHasEventPending(
        "AccountKeyChainActivated",
        watcher,
        user1.issuerKeyPair,
        fun
        | AccountKeyChainActivated({accountIdx, identifier, sequence})
            when AccountIndex.eq(accountIdx, AccountIndex.default) =>
          identifier
          == "8974ad69910afdca42d4c7c08c094c8d2a9d454d0f02b5b101eb7abd30a06d30"
          && sequence == 0
        | _ => false,
      );
    });
    describe("Is idle when the key chain has been activated", () => {
      let (user1, _user2) = G.twoUserSessions();
      let log =
        L.(
          Fixtures.createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
        );
      let acceptance =
        log |> L.lastEvent |> Event.getAccountCreationAcceptedExn;
      let log =
        L.(
          log
          |> withCustodian(user1, ~supporters=[user1])
          |> withCustodianKeyChain(user1)
          |> withAccountKeyChainIdentified
          |> withAccountKeyChainActivated(user1)
        );
      let watcher =
        AccountKeyChain.make(user1, acceptance, log |> L.eventLog);
      testWatcherHasNoEventPending(watcher);
    });
    describe("Activates a key chain when a custodian is removed", () => {
      let (user1, user2, _user3) = F.threeUserSessions;
      let log =
        L.(
          Fixtures.createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
        );
      let acceptance =
        log |> L.lastEvent |> Event.getAccountCreationAcceptedExn;
      let log =
        L.(
          log
          |> withCustodian(user1, ~supporters=[user1])
          |> withCustodianKeyChain(user1)
          |> withAccountKeyChainIdentified
          |> withAccountKeyChainActivated(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withCustodian(user2, ~supporters=[user1, user2])
          |> withCustodianKeyChain(user2)
          |> withAccountKeyChainIdentified
          |> withAccountKeyChainActivated(user1)
          |> withCustodianRemoved(user2, ~supporters=[user1])
        );
      let watcher =
        AccountKeyChain.make(user1, acceptance, log |> L.eventLog);
      testWatcherHasEventPending(
        "AccountKeyChainActivated",
        watcher,
        user1.issuerKeyPair,
        fun
        | AccountKeyChainActivated({accountIdx, identifier, sequence})
            when AccountIndex.eq(accountIdx, AccountIndex.default) =>
          identifier
          == "8974ad69910afdca42d4c7c08c094c8d2a9d454d0f02b5b101eb7abd30a06d30"
          && sequence == 1
        | _ => false,
      );
    });
    describe("Is idle when the partner is removed", () => {
      let (user1, user2) = G.twoUserSessions();
      let log =
        L.(
          Fixtures.createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
        );
      let acceptance =
        log |> L.lastEvent |> Event.getAccountCreationAcceptedExn;
      let log =
        L.(
          log
          |> withCustodian(user1, ~supporters=[user1])
          |> withCustodianKeyChain(user1)
          |> withAccountKeyChainIdentified
          |> withAccountKeyChainActivated(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withCustodian(user2, ~supporters=[user1, user2])
          |> withPartnerRemoved(user1, ~supporters=[user2])
          |> withCustodianKeyChain(user2)
        );
      let watcher =
        AccountKeyChain.make(user1, acceptance, log |> L.eventLog);
      testWatcherHasNoEventPending(watcher);
    });
    describe("Is idle when the custodian is removed", () => {
      let (user1, user2) = G.twoUserSessions();
      let log =
        L.(
          Fixtures.createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
        );
      let acceptance =
        log |> L.lastEvent |> Event.getAccountCreationAcceptedExn;
      let log =
        L.(
          log
          |> withCustodian(user1, ~supporters=[user1])
          |> withCustodianKeyChain(user1)
          |> withAccountKeyChainIdentified
          |> withAccountKeyChainActivated(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withCustodian(user2, ~supporters=[user1, user2])
          |> withCustodianRemoved(user1, ~supporters=[user2])
          |> withCustodianKeyChain(user2)
        );
      let watcher =
        AccountKeyChain.make(user1, acceptance, log |> L.eventLog);
      testWatcherHasNoEventPending(watcher);
    });
  });
