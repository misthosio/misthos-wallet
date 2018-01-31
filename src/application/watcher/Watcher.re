type t = {
  .
  receive: EventLog.item => unit,
  pendingEvent: unit => option((Bitcoin.ECPair.t, Event.t)),
  processCompleted: unit => bool
};

module PartnerApproval = Watcher__PartnerApproval;

module ContributionApproval = Watcher__ContributionApproval;

let initWatcherFor = ({event}: EventLog.item, log) =>
  switch event {
  | ProspectSuggested(suggestion) =>
    Some(PartnerApproval.make(suggestion, log))
  | ContributionSubmitted(submission) =>
    Some(ContributionApproval.make(submission, log))
  | _ => None
  };
