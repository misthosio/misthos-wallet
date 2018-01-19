type t = {
  .
  receive: EventLog.item => unit,
  pendingEvent: unit => option((Bitcoin.ECPair.t, Event.t)),
  processCompleted: unit => bool
};

module PartnerApproval = Watcher__PartnerApproval;

module ContributionApproval = {
  let make = (submission, log) => {
    val submission = submission;
    val approval = ref(1);
    pub receive = _event => approval := approval^ + 1;
    pub processCompleted = () => false;
    pub pendingEvent = () => None
  };
};

let initWatcherFor = ({event}: EventLog.item, log) =>
  switch event {
  | ProspectSuggested(suggestion) =>
    Some(PartnerApproval.make(suggestion, log))
  | ContributionSubmitted(submission) =>
    Some(ContributionApproval.make(submission, log))
  | _ => None
  };
