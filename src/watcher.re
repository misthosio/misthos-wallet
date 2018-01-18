open Event;

type t = {
  .
  receive: EventLog.item => unit,
  pendingEvent: unit => option((Bitcoin.ECPair.t, Event.t)),
  processCompleted: unit => bool
};

module ProspectApproval = {
  type state = {
    eligable: list(string),
    approvals: list(string),
    policy: Policy.t,
    systemIssuer: Bitcoin.ECPair.t
  };
  let make = (suggestion: ProspectSuggested.t, log) => {
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
            | DealCreated(event) => {
                ...state^,
                eligable: [event.creatorId],
                policy: event.metaPolicy,
                systemIssuer: event.systemIssuer
              }
            | ProspectApproved(event)
                when event.processId == suggestion.processId => {
                ...state^,
                approvals: [event.supporterId, ...state^.approvals]
              }
            | PartnerAdded(event) when event.processId == suggestion.processId =>
              completed := true;
              result := None;
              state^;
            | PartnerAdded(event) => {
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
              PartnerAdded({
                processId: suggestion.processId,
                blockstackId: suggestion.prospectId,
                pubKey: suggestion.prospectPubKey
              })
            ));
        };
      };
      pub processCompleted = () => completed^;
      pub pendingEvent = () => result^
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
    pub pendingEvent = () => None
  };
};

let initWatcherFor = ({event}: EventLog.item, log) =>
  switch event {
  | ProspectSuggested(suggestion) =>
    Some(ProspectApproval.make(suggestion, log))
  | ContributionSubmitted(submission) =>
    Some(ContributionApproval.make(submission, log))
  | _ => None
  };
