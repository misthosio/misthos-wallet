type watcher = {
  .
  receive: EventLog.item => unit,
  pendingEvent: unit => option(Js.Promise.t((Bitcoin.ECPair.t, Event.t))),
  processCompleted: unit => bool,
};

type t = list(watcher);

module Initialize = Watcher__InitializeVenture;

module PartnerApproval = Watcher__PartnerApproval;

module PartnerRemovalApproval = Watcher__PartnerRemovalApproval;

module AccountCreationApproval = Watcher__AccountCreationApproval;

module CustodianApproval = Watcher__CustodianApproval;

module CustodianRemovalApproval = Watcher__CustodianRemovalApproval;

module AutoEndorseCustodianSelf = Watcher__AutoEndorseCustodianSelf;

module CustodianKeyChain = Watcher__CustodianKeyChain;

module AccountKeyChain = Watcher__AccountKeyChain;

module AbortPayout = Watcher__AbortPayout;

module PayoutApproval = Watcher__PayoutApproval;

module SignPayout = Watcher__SignPayout;

module FinalizePayout = Watcher__FinalizePayout;

let initWatcherFor = (session, {event}: EventLog.item, log) =>
  switch (event) {
  | VentureCreated(event) => [Initialize.make(session, event, log)]
  | PartnerProposed(proposal) => [PartnerApproval.make(proposal, log)]
  | PartnerRemovalProposed(proposal) => [
      PartnerRemovalApproval.make(proposal, log),
    ]
  | PartnerAccepted(acceptance) => [
      AutoEndorseCustodianSelf.make(session, acceptance, log),
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
  | PayoutEndorsed(endorsement) => [
      SignPayout.make(session, endorsement, log),
    ]
  | PayoutAccepted(acceptance) => [FinalizePayout.make(acceptance, log)]
  | CustodianProposed(proposal) => [CustodianApproval.make(proposal, log)]
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

let rec processPendingPromise = (session, eventFound, promise) =>
  Js.Promise.(
    promise
    |> then_(((log, state, watchers)) => {
         let nextEvent =
           (
             try (
               Some(
                 watchers
                 |> List.find(w => w#pendingEvent() |> Js.Option.isSome),
               )
             ) {
             | Not_found => None
             }
           )
           |> Utils.mapOption(w => w#pendingEvent() |> Js.Option.getExn);
         switch (nextEvent) {
         | None => (log, state, watchers) |> resolve
         | Some(eventPromise) =>
           processPendingPromise(
             session,
             eventFound,
             eventPromise
             |> then_(((issuer, event)) => {
                  let (item, log, state) =
                    state |> eventFound(issuer, event, log);
                  (log, state, watchers |> apply(session, item, log))
                  |> resolve;
                }),
           )
         };
       })
  );

let processPending = (session, log, eventFound, state, watchers) =>
  processPendingPromise(
    session,
    eventFound,
    (log, state, watchers) |> Js.Promise.resolve,
  );

let applyAndProcessPending = (session, item, log, eventFound, state, watchers) =>
  watchers
  |> apply(session, item, log)
  |> processPending(session, log, eventFound, state);
