open PrimitiveTypes;

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
  describe("Updates the key chain when a custodian key chain changes", () => {
    let user1 = G.userSession("user1" |> UserId.fromString);
    let log =
      L.(
        createVenture(user1)
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
    let watcher = AccountKeyChain.make(acceptance, log |> L.eventLog);
    testWatcherHasEventPending(
      "AccountKeyChainUpdated",
      watcher,
      log |> L.systemIssuer,
      fun
      | AccountKeyChainUpdated({
          keyChain: {accountIdx, keyChainIdx, nCoSigners, custodianKeyChains},
        })
          when AccountIndex.eq(accountIdx, AccountIndex.default) =>
        AccountKeyChainIndex.eq(keyChainIdx, AccountKeyChainIndex.first)
        && nCoSigners == 1
        && custodianKeyChains
        |> List.length == 1
      | _ => false,
    );
  });
  describe("Increases the AccountKeyChainIndex every time", () => {
    let (user1, user2) = G.twoUserSessions();
    let log =
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withAccount(~supporter=user1)
      );
    let acceptance = log |> L.lastEvent |> Event.getAccountCreationAcceptedExn;
    let log =
      L.(
        log
        |> withCustodian(user1, ~supporters=[user1])
        |> withCustodianKeyChain(user1)
        |> withAccountKeyChain
        |> withPartner(user2, ~supporters=[user1])
        |> withCustodian(user2, ~supporters=[user1, user2])
        |> withCustodianKeyChain(user2)
      );
    let watcher = AccountKeyChain.make(acceptance, log |> L.eventLog);
    testWatcherHasEventPending(
      "AccountKeyChainUpdated",
      watcher,
      log |> L.systemIssuer,
      fun
      | AccountKeyChainUpdated({
          keyChain: {accountIdx, keyChainIdx, nCoSigners, custodianKeyChains},
        })
          when AccountIndex.eq(accountIdx, AccountIndex.default) =>
        AccountKeyChainIndex.eq(
          keyChainIdx,
          AccountKeyChainIndex.first |> AccountKeyChainIndex.next,
        )
        && nCoSigners == 1
        && custodianKeyChains
        |> List.length == 2
      | _ => false,
    );
  });
};
