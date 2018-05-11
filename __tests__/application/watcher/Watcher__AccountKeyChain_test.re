open Jest;

module KeyChain = CustodianKeyChain;

module AccountKeyChain = Watcher__AccountKeyChain;

module G = Generators;

module E = G.Event;

module L = G.Log;

open WalletTypes;

open WatcherHelpers;

let keyChainEq = (keyChainA, keyChainB) =>
  keyChainA |> KeyChain.encode == (keyChainB |> KeyChain.encode);

let () = {
  describe("Identifies a key chain when a custodian key chain changes", () => {
    let (user1, _, _) = Fixtures.threeUserSessions;
    let log =
      L.(
        Fixtures.createVenture(user1)
        |> withFirstPartner(user1)
        |> withAccount(~supporter=user1)
      );
    let acceptance = log |> L.lastEvent |> Event.getAccountCreationAcceptedExn;
    let log =
      L.(
        log
        |> withCustodian(user1, ~supporters=[user1])
        |> withCustodianKeyChain(user1)
      );
    let watcher = AccountKeyChain.make(user1, acceptance, log |> L.eventLog);
    testWatcherHasEventPending(
      "AccountKeyChainIdentified",
      watcher,
      log |> L.systemIssuer,
      fun
      | AccountKeyChainIdentified({keyChain: {identifier, accountIdx}})
          when AccountIndex.eq(accountIdx, AccountIndex.default) =>
        identifier
        == "41f508a17ccd3b6e325be410341fd320d8d72befbb54cddf5723432a340bcc73"
      | _ => false,
    );
  });
  describe("Identifies a key chain when a partner is removed", () => {
    let (user1, user2, _) = Fixtures.threeUserSessions;
    let log =
      L.(
        Fixtures.createVenture(user1)
        |> withFirstPartner(user1)
        |> withAccount(~supporter=user1)
      );
    let acceptance = log |> L.lastEvent |> Event.getAccountCreationAcceptedExn;
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
    let watcher = AccountKeyChain.make(user1, acceptance, log |> L.eventLog);
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
        == "038ccb176653ad573f4342fc625dc121b573762a69becbaaad5b8bbadb934340"
        && custodianKeyChains
        |> List.length == 1
      | _ => false,
    );
  });
  describe("Activates a key chain when a custodian key chain changes", () => {
    let (user1, _, _) = Fixtures.threeUserSessions;
    let log =
      L.(
        Fixtures.createVenture(user1)
        |> withFirstPartner(user1)
        |> withAccount(~supporter=user1)
      );
    let acceptance = log |> L.lastEvent |> Event.getAccountCreationAcceptedExn;
    let log =
      L.(
        log
        |> withCustodian(user1, ~supporters=[user1])
        |> withCustodianKeyChain(user1)
        |> withAccountKeyChainIdentified
      );
    let watcher = AccountKeyChain.make(user1, acceptance, log |> L.eventLog);
    testWatcherHasEventPending(
      "AccountKeyChainActivated",
      watcher,
      user1.issuerKeyPair,
      fun
      | AccountKeyChainActivated({accountIdx, identifier, sequence})
          when AccountIndex.eq(accountIdx, AccountIndex.default) =>
        identifier
        == "41f508a17ccd3b6e325be410341fd320d8d72befbb54cddf5723432a340bcc73"
        && sequence == 0
      | _ => false,
    );
  });
  describe("Is idle when the key chain has been activated", () => {
    let (user1, _, _) = Fixtures.threeUserSessions;
    let log =
      L.(
        Fixtures.createVenture(user1)
        |> withFirstPartner(user1)
        |> withAccount(~supporter=user1)
      );
    let acceptance = log |> L.lastEvent |> Event.getAccountCreationAcceptedExn;
    let log =
      L.(
        log
        |> withCustodian(user1, ~supporters=[user1])
        |> withCustodianKeyChain(user1)
        |> withAccountKeyChainIdentified
        |> withAccountKeyChainActivated(user1)
      );
    let watcher = AccountKeyChain.make(user1, acceptance, log |> L.eventLog);
    testWatcherHasNoEventPending(watcher);
  });
  describe("Activates a key chain when a custodian is removed", () => {
    let (user1, user2, _) = Fixtures.threeUserSessions;
    let log =
      L.(
        Fixtures.createVenture(user1)
        |> withFirstPartner(user1)
        |> withAccount(~supporter=user1)
      );
    let acceptance = log |> L.lastEvent |> Event.getAccountCreationAcceptedExn;
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
    let watcher = AccountKeyChain.make(user1, acceptance, log |> L.eventLog);
    testWatcherHasEventPending(
      "AccountKeyChainActivated",
      watcher,
      user1.issuerKeyPair,
      fun
      | AccountKeyChainActivated({accountIdx, identifier, sequence})
          when AccountIndex.eq(accountIdx, AccountIndex.default) =>
        identifier
        == "41f508a17ccd3b6e325be410341fd320d8d72befbb54cddf5723432a340bcc73"
        && sequence == 1
      | _ => false,
    );
  });
  describe("Is idle when the partner is removed", () => {
    let (user1, user2, _) = Fixtures.threeUserSessions;
    let log =
      L.(
        Fixtures.createVenture(user1)
        |> withFirstPartner(user1)
        |> withAccount(~supporter=user1)
      );
    let acceptance = log |> L.lastEvent |> Event.getAccountCreationAcceptedExn;
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
    let watcher = AccountKeyChain.make(user1, acceptance, log |> L.eventLog);
    testWatcherHasNoEventPending(watcher);
  });
};
