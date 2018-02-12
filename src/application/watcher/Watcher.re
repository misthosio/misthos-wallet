type t = {
  .
  receive: EventLog.item => unit,
  pendingEvent: unit => option((Bitcoin.ECPair.t, Event.t)),
  processCompleted: unit => bool
};

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
