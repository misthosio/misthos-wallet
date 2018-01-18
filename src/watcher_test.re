open Jest;

open Expect;

open Event;

let () =
  describe("CandidateApproval", () => {
    let issuer = Bitcoin.ECPair.makeRandom();
    let projectCreated =
      Event.DealCreated.make(
        ~projectName="TheMothers",
        ~creatorId="frank.id",
        ~creatorPubKey=issuer |> Utils.publicKeyFromKeyPair,
        ~metaPolicy=Policy.absolute
      );
    let (_, log) =
      EventLog.make() |> EventLog.append(issuer, DealCreated(projectCreated));
    let candidateId = "wackerman.id";
    let candidatePubKey = "sticks";
    let candidateSuggestion =
      Event.CandidateSuggested.make(
        ~supporterId="bozzio.id",
        ~candidateId,
        ~candidatePubKey
      );
    let processId = candidateSuggestion.processId;
    test("Process is in progress", () => {
      let (item, log) =
        log |> EventLog.append(issuer, CandidateSuggested(candidateSuggestion));
      let candidateWatcher =
        Watcher.initWatcherFor(item, log) |> Js.Option.getExn;
      expect(candidateWatcher#processCompleted()) |> toBe(false);
    });
    test("completes when Member is added", () => {
      let (item, log) =
        log |> EventLog.append(issuer, CandidateSuggested(candidateSuggestion));
      let candidateWatcher =
        Watcher.initWatcherFor(item, log) |> Js.Option.getExn;
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
      let (item, log) =
        log |> EventLog.append(issuer, CandidateSuggested(candidateSuggestion));
      let candidateWatcher =
        Watcher.initWatcherFor(item, log) |> Js.Option.getExn;
      let (item, _) =
        log
        |> EventLog.append(
             issuer,
             CandidateApproved(
               Event.CandidateApproved.make(
                 ~processId,
                 ~candidateId,
                 ~supporterId="frank.id"
               )
             )
           );
      candidateWatcher#receive(item);
      expect(candidateWatcher#pendingEvent())
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
    test("Does not complete without absolute approval", () => {
      let (_, log) =
        log
        |> EventLog.append(
             issuer,
             MemberAdded({
               processId: Uuid.v4(),
               blockstackId: "ruth.id",
               pubKey: "mallets"
             })
           );
      let (item, log) =
        log |> EventLog.append(issuer, CandidateSuggested(candidateSuggestion));
      let candidateWatcher =
        Watcher.initWatcherFor(item, log) |> Js.Option.getExn;
      let (item, _) =
        log
        |> EventLog.append(
             issuer,
             CandidateApproved(
               Event.CandidateApproved.make(
                 ~processId,
                 ~candidateId,
                 ~supporterId="frank.id"
               )
             )
           );
      candidateWatcher#receive(item);
      expect(candidateWatcher#pendingEvent()) |> toBe(None);
    });
  });
