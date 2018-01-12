open Event;

type t = {
  .
  receive: EventLog.item => unit,
  resultingEvent: unit => option((Bitcoin.ECPair.t, Event.t)),
  processCompleted: unit => bool
};

module CandidateApproval = {
  type state = {
    eligable: list(string),
    approvals: list(string),
    policy: Policy.t,
    systemIssuer: Bitcoin.ECPair.t
  };
  let make = (suggestion: CandidateSuggested.t, log) => {
    let process = {
      val state =
        ref({
          eligable: [],
          approvals: [suggestion.supporterId],
          policy: Policy.absolute,
          systemIssuer: Bitcoin.ECPair.makeRandom()
        });
      val completed = ref(false);
      val result = ref(None);
      pub receive = ({event}: EventLog.item) => {
        state :=
          (
            switch event {
            | ProjectCreated(event) => {
                ...state^,
                eligable: [event.creatorId],
                policy: event.metaPolicy,
                systemIssuer: event.systemIssuer
              }
            | CandidateApproved(event)
                when event.processId == suggestion.processId => {
                ...state^,
                approvals: [event.supporterId, ...state^.approvals]
              }
            | MemberAdded(event) when event.processId == suggestion.processId =>
              completed := true;
              result := None;
              state^;
            | MemberAdded(event) => {
                ...state^,
                eligable: [event.blockstackId, ...state^.eligable]
              }
            | _ => state^
            }
          );
        if (state^.policy
            |> Policy.fulfilled(
                 ~eligable=state^.eligable,
                 ~approved=state^.approvals
               )) {
          result :=
            Some((
              state^.systemIssuer,
              MemberAdded({
                processId: suggestion.processId,
                blockstackId: suggestion.candidateId,
                pubKey: suggestion.candidatePubKey
              })
            ));
        };
      };
      pub processCompleted = () => completed^;
      pub resultingEvent = () => result^
    };
    log |> EventLog.reduce((_, item) => process#receive(item), ());
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

let initWatcherFor = ({event}: EventLog.item, log) =>
  switch event {
  | CandidateSuggested(suggestion) =>
    Some(CandidateApproval.make(suggestion, log))
  | ContributionSubmitted(submission) =>
    Some(ContributionApproval.make(submission, log))
  | _ => None
  };
