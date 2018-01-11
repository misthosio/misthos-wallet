open Jest;

open Expect;

open Event;

let () =
  describe("CandidateApproval", () => {
    let issuer = Bitcoin.ECPair.makeRandom();
    let projectCreated =
      Event.ProjectCreated.make(
        ~projectName="TheMothers",
        ~creatorId="frank.id",
        ~creatorPubKey=issuer |> Utils.publicKeyFromKeyPair,
        ~metaPolicy=Policy.absolute
      );
    let (_, log) =
      EventLog.make()
      |> EventLog.append(issuer, ProjectCreated(projectCreated));
    let candidateId = "wackerman.id";
    let candidatePubKey = "sticks";
    let candidateSuggestion =
      Event.CandidateSuggested.make(
        ~supporterId="bozzio.id",
        ~candidateId,
        ~candidatePubKey
      );
    let processId = candidateSuggestion.processId;
    test("started", () => {
      let candidateWatcher =
        Watcher.CandidateApproval.make(candidateSuggestion, log);
      expect(candidateWatcher#processCompleted()) |> toBe(false);
    });
    test("completes when Member is added", () => {
      let candidateWatcher =
        Watcher.CandidateApproval.make(candidateSuggestion, log);
      let (item, _) =
        EventLog.append(
          issuer,
          MemberAdded({
            processId,
            blockstackId: candidateId,
            pubKey: candidatePubKey
          }),
          log
        );
      candidateWatcher#receive(item);
      expect(candidateWatcher#processCompleted()) |> toBe(true);
    });
    test("Issues an event when approval is reached", () => {
      let candidateWatcher =
        Watcher.CandidateApproval.make(candidateSuggestion, log);
      let (item, _) =
        EventLog.append(
          issuer,
          CandidateApproved(
            Event.CandidateApproved.make(
              ~processId,
              ~candidateId,
              ~supporterId="frank.id"
            )
          ),
          log
        );
      candidateWatcher#receive(item);
      expect(candidateWatcher#resultingEvent())
      |> toEqual(
           Some((
             projectCreated.systemIssuer,
             MemberAdded({
               processId,
               blockstackId: candidateId,
               pubKey: candidatePubKey
             })
           ))
         );
    });
  });
