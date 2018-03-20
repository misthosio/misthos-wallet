type watcher = {
  .
  receive: EventLog.item => unit,
  pendingEvent: unit => option((Bitcoin.ECPair.t, Event.t)),
  processCompleted: unit => bool
};

type t = list(watcher);

module PartnerApproval = Watcher__PartnerApproval;

module PartnerLabelApproval = Watcher__PartnerLabelApproval;

module ContributionApproval = Watcher__ContributionApproval;

let initWatcherFor = ({event}: EventLog.item, log) =>
  switch event {
  | PartnerProposed(proposal) => Some(PartnerApproval.make(proposal, log))
  | PartnerLabelProposed(proposal) =>
    Some(PartnerLabelApproval.make(proposal, log))
  | ContributionProposed(proposal) =>
    Some(ContributionApproval.make(proposal, log))
  | _ => None
  };

let apply = (~reconstruct=false, item, log, watchers) => {
  /* To prevent watchers receiving items twice on reconstruction */
  if (reconstruct == false) {
    watchers |> List.iter(w => w#receive(item));
  };
  (
    switch (initWatcherFor(item, log)) {
    | Some(w) => [w, ...watchers]
    | None => watchers
    }
  )
  |> List.filter(w => w#processCompleted() == false);
};

let rec processPending = (log, eventFound, state, watchers) => {
  let nextEvent =
    (
      try (
        Some(
          watchers
          |> List.rev
          |> List.find(w => w#pendingEvent() |> Js.Option.isSome)
        )
      ) {
      | Not_found => None
      }
    )
    |> DoNotFormat.andThenGetEvent;
  switch nextEvent {
  | None => (log, state, watchers)
  | Some((issuer, event)) =>
    let (item, log, state) = state |> eventFound(issuer, event, log);
    watchers |> apply(item, log) |> processPending(log, eventFound, state);
  };
};

let applyAndProcessPending = (item, log, eventFound, state, watchers) =>
  watchers |> apply(item, log) |> processPending(log, eventFound, state);
