type watcher = {
  .
  receive: EventLog.item => unit,
  pendingEvent: unit => option((Bitcoin.ECPair.t, Event.t)),
  processCompleted: unit => bool,
};

type t = list(watcher);

module Initialize = Watcher__InitializeVenture;
module PartnerApproval = Watcher__PartnerApproval;
module AddPubKey = Watcher__AddPubKey;
module PartnerRemovalApproval = Watcher__PartnerRemovalApproval;
module AccountCreationApproval = Watcher__AccountCreationApproval;
module CustodianApproval = Watcher__CustodianApproval;
module CustodianRemovalApproval = Watcher__CustodianRemovalApproval;
module AutoEndorseCustodianSelf = Watcher__AutoEndorseCustodianSelf;
module CustodianKeyChain = Watcher__CustodianKeyChain;
module AccountKeyChain = Watcher__AccountKeyChain;
module AbortPayout = Watcher__AbortPayout;
module PayoutApproval = Watcher__PayoutApproval;
module FinalizePayout = Watcher__FinalizePayout;

let initWatcherFor = (session, {event}: EventLog.item, log) =>
  switch (event) {
  | VentureCreated(event) => [Initialize.make(session, event, log)]
  | PartnerProposed(proposal) => [PartnerApproval.make(proposal, log)]
  | PartnerAccepted(acceptance) => [
      AddPubKey.make(session, acceptance, log),
    ]
  | PartnerRemovalProposed(proposal) => [
      PartnerRemovalApproval.make(proposal, log),
    ]
  | AccountCreationProposed(proposal) => [
      AccountCreationApproval.make(proposal, log),
    ]
  | AccountCreationAccepted(acceptance) => [
      AccountKeyChain.make(session, acceptance, log),
    ]
  | PayoutProposed(proposal) => [
      PayoutApproval.make(proposal, log),
      AbortPayout.make(proposal, log),
    ]
  | PayoutAccepted(acceptance) => [FinalizePayout.make(acceptance, log)]
  | CustodianProposed(proposal) => [
      AutoEndorseCustodianSelf.make(session, proposal, log),
      CustodianApproval.make(proposal, log),
    ]
  | CustodianRemovalProposed(proposal) => [
      CustodianRemovalApproval.make(proposal, log),
    ]
  | CustodianAccepted(acceptance) => [
      CustodianKeyChain.make(session, acceptance, log),
    ]
  | _ => []
  };

let apply = (~reconstruct=false, session, item, log, watchers) =>
  switch (item) {
  | Some(item) =>
    /* To prevent watchers receiving items twice on reconstruction */
    if (reconstruct == false) {
      watchers |> List.iter(w => w#receive(item));
    };
    List.append(initWatcherFor(session, item, log), watchers)
    |> List.filter(w => w#processCompleted() == false);
  | None => watchers
  };

let rec processPending = (session, eventFound, (log, state, watchers)) => {
  let nextEvent =
    (
      try (
        Some(watchers |> List.find(w => w#pendingEvent() |> Js.Option.isSome))
      ) {
      | Not_found => None
      }
    )
    |> Utils.mapOption(w => w#pendingEvent() |> Js.Option.getExn);
  switch (nextEvent) {
  | None => (log, state, watchers)
  | Some((issuer, event)) =>
    processPending(
      session,
      eventFound,
      {
        let (item, log, state) = state |> eventFound(issuer, event, log);
        (log, state, watchers |> apply(session, item, log));
      },
    )
  };
};

let processPending = (session, log, eventFound, state, watchers) =>
  processPending(session, eventFound, (log, state, watchers));

let applyAndProcessPending = (session, item, log, eventFound, state, watchers) =>
  watchers
  |> apply(session, item, log)
  |> processPending(session, log, eventFound, state);
