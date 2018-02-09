open Event;

open PrimitiveTypes;

type state = {
  eligable: list(userId),
  approvals: list(userId),
  policy: Policy.t,
  systemIssuer: Bitcoin.ECPair.t
};

let make = (suggestion: ProspectSuggested.t, log) => {
  let process = {
    /*eslint-disable */
    val state =
      ref({
        eligable: [],
        approvals: [suggestion.supporterId],
        policy: Policy.absolute,
        systemIssuer: Bitcoin.ECPair.makeRandom()
      });
    /*eslint-enable */
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
              when ProcessId.eq(event.processId, suggestion.processId) => {
              ...state^,
              approvals: [event.supporterId, ...state^.approvals]
            }
          | PartnerAdded(event)
              when ProcessId.eq(event.processId, suggestion.processId) =>
            completed := true;
            state^;
          | PartnerAdded(event) => {
              ...state^,
              eligable: [event.partnerId, ...state^.eligable]
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
                ~partnerId=suggestion.prospectId,
                ~partnerPubKey=suggestion.prospectPubKey
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
