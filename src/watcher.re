type t = {
  .
  receive: Event.t => unit,
  processCompleted: unit => bool
};

module CandidateApproval = {
  let make = suggestion => {
    val suggestion = suggestion;
    val approval = ref(1);
    pub receive = _event => approval := approval^ + 1;
    pub processCompleted = () => false
  };
};

module ContributionApproval = {
  let make = submission => {
    val submission = submission;
    val approval = ref(1);
    pub receive = _event => approval := approval^ + 1;
    pub processCompleted = () => false
  };
};

let addWatcher = (event: Event.t, watchers) =>
  switch event {
  | CandidateSuggested(suggestion) => [
      CandidateApproval.make(suggestion),
      ...watchers
    ]
  | ContributionSubmitted(submission) => [
      ContributionApproval.make(submission),
      ...watchers
    ]
  | _ => watchers
  };
