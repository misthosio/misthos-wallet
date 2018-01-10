open Jest;

open Expect;

open Event;

let () =
  describe("CandidateApproval", () => {
    let issuer = Bitcoin.ECPair.makeRandom();
    let log =
      EventLog.make()
      |> EventLog.append(
           ProjectCreated(
             Event.ProjectCreated.make(
               ~projectName="TheMothers",
               ~creatorId="frank.id",
               ~creatorPubKey=issuer |> Utils.publicKeyFromKeyPair,
               ~metaPolicy=Policy.absolute
             )
           ),
           issuer
         );
    let candidateId = "bozzio.id";
    let candidatePubKey = "sticks";
    let candidateSuggestion =
      Event.CandidateSuggested.make(~candidateId, ~candidatePubKey);
    let processId = candidateSuggestion.processId;
    test("started", () => {
      let candidateWatcher =
        Watcher.CandidateApproval.make(candidateSuggestion, log);
      expect(candidateWatcher#processCompleted()) |> toBe(false);
    });
    test("completes when Member is added", () => {
      let candidateWatcher =
        Watcher.CandidateApproval.make(candidateSuggestion, log);
      candidateWatcher#receive(
        MemberAdded({
          processId,
          blockstackId: candidateId,
          pubKey: candidatePubKey
        })
      );
      expect(candidateWatcher#processCompleted()) |> toBe(true);
    });
    test("Issues an event when approval is reached", () => {
      let candidateWatcher =
        Watcher.CandidateApproval.make(candidateSuggestion, log);
      candidateWatcher#receive(
        CandidateApproved(
          Event.CandidateApproved.make(
            ~processId,
            ~candidateId,
            ~supporterId="frank.id"
          )
        )
      );
      expect(candidateWatcher#resultingEvent())
      |> toEqual(
           Some(
             MemberAdded({
               processId,
               blockstackId: candidateId,
               pubKey: candidatePubKey
             })
           )
         );
    });
  });
