open Jest;

open Expect;

open Event;

let () =
  describe("ProspectApproval", () => {
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
    let prospectId = "wackerman.id";
    let prospectPubKey = "sticks";
    let prospectSuggestion =
      Event.ProspectSuggested.make(
        ~supporterId="bozzio.id",
        ~prospectId,
        ~prospectPubKey
      );
    let processId = prospectSuggestion.processId;
    test("Process is in progress", () => {
      let (item, log) =
        log |> EventLog.append(issuer, ProspectSuggested(prospectSuggestion));
      let prospectWatcher =
        Watcher.initWatcherFor(item, log) |> Js.Option.getExn;
      expect(prospectWatcher#processCompleted()) |> toBe(false);
    });
    test("completes when Partner is added", () => {
      let (item, log) =
        log |> EventLog.append(issuer, ProspectSuggested(prospectSuggestion));
      let prospectWatcher =
        Watcher.initWatcherFor(item, log) |> Js.Option.getExn;
      let (item, _) =
        EventLog.append(
          issuer,
          PartnerAdded({
            processId,
            blockstackId: prospectId,
            pubKey: prospectPubKey
          }),
          log
        );
      prospectWatcher#receive(item);
      expect(prospectWatcher#processCompleted()) |> toBe(true);
    });
    test("Issues an event when approval is reached", () => {
      let (item, log) =
        log |> EventLog.append(issuer, ProspectSuggested(prospectSuggestion));
      let prospectWatcher =
        Watcher.initWatcherFor(item, log) |> Js.Option.getExn;
      let (item, _) =
        log
        |> EventLog.append(
             issuer,
             ProspectApproved(
               Event.ProspectApproved.make(
                 ~processId,
                 ~prospectId,
                 ~supporterId="frank.id"
               )
             )
           );
      prospectWatcher#receive(item);
      expect(prospectWatcher#pendingEvent())
      |> toEqual(
           Some((
             projectCreated.systemIssuer,
             PartnerAdded({
               processId,
               blockstackId: prospectId,
               pubKey: prospectPubKey
             })
           ))
         );
    });
    test("Does not complete without absolute approval", () => {
      let (_, log) =
        log
        |> EventLog.append(
             issuer,
             PartnerAdded({
               processId: Uuid.v4(),
               blockstackId: "ruth.id",
               pubKey: "mallets"
             })
           );
      let (item, log) =
        log |> EventLog.append(issuer, ProspectSuggested(prospectSuggestion));
      let prospectWatcher =
        Watcher.initWatcherFor(item, log) |> Js.Option.getExn;
      let (item, _) =
        log
        |> EventLog.append(
             issuer,
             ProspectApproved(
               Event.ProspectApproved.make(
                 ~processId,
                 ~prospectId,
                 ~supporterId="frank.id"
               )
             )
           );
      prospectWatcher#receive(item);
      expect(prospectWatcher#pendingEvent()) |> toBe(None);
    });
  });
