open PrimitiveTypes;

open Jest;

module AccountCreationApproval = Watcher__AccountCreationApproval;

module G = Generators;

module E = G.Event;

module L = G.Log;

open WatcherHelpers;

let () = {
  describe("With 1 partner and a proposal", () => {
    let user1 = G.userSession("user1" |> UserId.fromString);
    let log =
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withAccountCreationProposed(~supporter=user1)
      );
    let proposal = log |> L.lastEvent |> Event.getAccountCreationProposedExn;
    let watcher = AccountCreationApproval.make(proposal, log |> L.eventLog);
    testWatcherHasEventPending(
      "AccountCreationAccepted",
      watcher,
      log |> L.systemIssuer,
      fun
      | AccountCreationAccepted({data}) => data == proposal.data
      | _ => false,
    );
  });
  describe("Completes when the account is accepted", () => {
    let user1 = G.userSession("user1" |> UserId.fromString);
    let log =
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withAccountCreationProposed(~supporter=user1)
      );
    let proposal = log |> L.lastEvent |> Event.getAccountCreationProposedExn;
    let watcher = AccountCreationApproval.make(proposal, log |> L.eventLog);
    let log = log |> L.withAccountCreationAccepted(proposal);
    watcher#receive(log |> L.lastItem);
    testWatcherHasCompleted(watcher);
  });
};
