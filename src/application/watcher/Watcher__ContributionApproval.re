open Event;

type state = {
  eligable: list(string),
  approvals: list(string),
  policy: Policy.t,
  systemIssuer: Bitcoin.ECPair.t
};

let make = (submission: ContributionSubmitted.t, log) => {
  let process = {
    val state =
      ref({
        eligable: [],
        approvals: [submission.submitterId],
        policy: Policy.absolute,
        systemIssuer: Bitcoin.ECPair.makeRandom()
      });
    val completed = ref(false);
    val result = ref(None);
    pub receive = ({event}: EventLog.item) => {
      state :=
        (
          switch event {
          | VentureCreated(event) => {
              ...state^,
              eligable: [event.creatorId],
              policy: event.metaPolicy,
              systemIssuer: event.systemIssuer
            }
          | PartnerAdded(event) => {
              ...state^,
              eligable: [event.blockstackId, ...state^.eligable]
            }
          | ContributionApproved(event)
              when event.processId == submission.processId => {
              ...state^,
              approvals: [event.supporterId, ...state^.approvals]
            }
          | ContributionAccepted(event)
              when event.processId == submission.processId =>
            completed := true;
            state^;
          | _ => state^
          }
        );
      result := None;
      if (completed^ == false
          && state^.policy
          |> Policy.fulfilled(
               ~eligable=state^.eligable,
               ~approved=state^.approvals
             )) {
        result :=
          Some((
            state^.systemIssuer,
            ContributionAccepted(
              ContributionAccepted.make(~processId=submission.processId)
            )
          ));
      };
    };
    pub processCompleted = () => completed^;
    pub pendingEvent = () => result^
  };
  log |> EventLog.reduce((_, item) => process#receive(item), ());
  process;
};
