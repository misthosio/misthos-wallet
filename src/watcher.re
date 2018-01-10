open Event;

type t = {
  .
  receive: Event.t => unit,
  resultingEvent: unit => option(Event.t),
  processCompleted: unit => bool
};

module CandidateApproval = {
  type state = {
    eligable: list(string),
    approvals: list(string),
    policy: Policy.t
  };
  let make = (suggestion: CandidateSuggested.t, log) => {
    let process = {
      val state =
        ref({
          eligable: [],
          approvals: [suggestion.supporterId],
          policy: Policy.absolute
        });
      val completed = ref(false);
      val result = ref(None);
      pub receive = event => {
        switch event {
        | ProjectCreated(event) =>
          state :=
            {...state^, eligable: [event.creatorId], policy: event.metaPolicy}
        | CandidateApproved(event) when event.processId == suggestion.processId =>
          state :=
            {...state^, approvals: [event.supporterId, ...state^.approvals]}
        | MemberAdded(event) when event.processId == suggestion.processId =>
          completed := true;
          result := None;
        | _ => ()
        };
        if (state^.policy
            |> Policy.fulfilled(
                 ~eligable=state^.eligable,
                 ~approved=state^.approvals
               )) {
          result :=
            Some(
              MemberAdded({
                processId: suggestion.processId,
                blockstackId: suggestion.candidateId,
                pubKey: suggestion.candidatePubKey
              })
            );
        };
      };
      pub processCompleted = () => completed^;
      pub resultingEvent = () => result^
    };
    log |> EventLog.reduce((_, (_, event)) => process#receive(event), ());
    process;
  };
};

module ContributionApproval = {
  let make = (submission, log) => {
    val submission = submission;
    val approval = ref(1);
    pub receive = _event => approval := approval^ + 1;
    pub processCompleted = () => false;
    pub resultingEvent = () => None
  };
};

let initWatcherFor = (event: Event.t, log) =>
  switch event {
  | CandidateSuggested(suggestion) =>
    Some(CandidateApproval.make(suggestion, log))
  | ContributionSubmitted(submission) =>
    Some(ContributionApproval.make(submission, log))
  | _ => None
  };
