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
  | PartnerProposed(proposal) => Some(PartnerApproval.make(proposal, log))
  | ContributionSubmitted(submission) =>
    Some(ContributionApproval.make(submission, log))
  | _ => None
  };
