open Jest;

open Expect;

let () =
  describe("CandidateApproval", () => {
    let candidateSuggestion =
      Event.CandidateSuggested.make(
        ~candidateId="id",
        ~candidatePubKey="pubKey"
      );
    let processId = candidateSuggestion.processId;
    test("done", () => {
      let candidateSuggestion =
        Event.CandidateSuggested.make(
          ~candidateId="id",
          ~candidatePubKey="pubKey"
        );
      let candidateWatcher =
        Watcher.CandidateApproval.make(candidateSuggestion);
      expect(candidateWatcher#processCompleted()) |> toBe(false);
    });
  });
