open Event;

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
          | VentureCreated(event) => {
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
            state^;
          | PartnerAdded(event) => {
              ...state^,
              eligable: [event.blockstackId, ...state^.eligable]
            }
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
            PartnerAdded(
              PartnerAdded.make(
                ~processId=suggestion.processId,
                ~blockstackId=suggestion.prospectId,
                ~pubKey=suggestion.prospectPubKey
              )
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
