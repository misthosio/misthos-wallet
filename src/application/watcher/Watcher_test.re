open Jest;

open Expect;

open Event;

open PrimitiveTypes;

let () =
  describe("PartnerApproval", () => {
    let issuer = Bitcoin.ECPair.makeRandom();
    let ventureCreated =
      Event.VentureCreated.make(
        ~ventureName="TheMothers",
        ~creatorId="frank.id" |> UserId.fromString,
        ~creatorPubKey=issuer |> Utils.publicKeyFromKeyPair,
        ~metaPolicy=Policy.absolute
      );
    let (_, log) =
      EventLog.make()
      |> EventLog.append(issuer, VentureCreated(ventureCreated));
    let prospectId = "wackerman.id" |> UserId.fromString;
    let prospectPubKey = "sticks";
    let prospectSuggestion =
      Event.ProspectSuggested.make(
        ~supporterId="bozzio.id" |> UserId.fromString,
        ~prospectId,
        ~prospectPubKey,
        ~policy=Policy.absolute
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
            partnerId: prospectId,
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
                 ~supporterId="frank.id" |> UserId.fromString
               )
             )
           );
      prospectWatcher#receive(item);
      expect(prospectWatcher#pendingEvent())
      |> toEqual(
           Some((
             ventureCreated.systemIssuer,
             PartnerAdded({
               processId,
               partnerId: prospectId,
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
               processId: ProcessId.make(),
               partnerId: "ruth.id" |> UserId.fromString,
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
                 ~supporterId="frank.id" |> UserId.fromString
               )
             )
           );
      prospectWatcher#receive(item);
      expect(prospectWatcher#pendingEvent()) |> toBe(None);
    });
  });
