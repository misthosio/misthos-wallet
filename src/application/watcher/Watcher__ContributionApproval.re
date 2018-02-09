open Event;

open PrimitiveTypes;

type state = {
  eligable: list(userId),
  endorsals: list(userId),
  policy: Policy.t,
  systemIssuer: Bitcoin.ECPair.t
};

let make = (submission: ContributionSubmitted.t, log) => {
  let process = {
    val state =
      ref({
        eligable: [],
        endorsals: [submission.submitterId],
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
              eligable: [event.partnerId, ...state^.eligable]
            }
          | ContributionEndorsed(event)
              when ProcessId.eq(event.processId, submission.processId) => {
              ...state^,
              endorsals: [event.supporterId, ...state^.endorsals]
            }
          | ContributionAccepted(event)
              when ProcessId.eq(event.processId, submission.processId) =>
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
               ~endorsed=state^.endorsals
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
