open Event;

open PrimitiveTypes;

type state = {
  eligable: list(userId),
  endorsements: list(userId),
  policy: Policy.t,
  systemIssuer: Bitcoin.ECPair.t
};

let make = (proposal: Contribution.Proposal.t, log) => {
  let process = {
    val state =
      ref({
        eligable: [],
        endorsements: [proposal.supporterId],
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
              policy: event.metaPolicy,
              systemIssuer: event.systemIssuer
            }
          | PartnerAccepted({data}) => {
              ...state^,
              eligable: [data.id, ...state^.eligable]
            }
          | ContributionEndorsed(event)
              when ProcessId.eq(event.processId, proposal.processId) => {
              ...state^,
              endorsements: [event.supporterId, ...state^.endorsements]
            }
          | ContributionAccepted(event)
              when ProcessId.eq(event.processId, proposal.processId) =>
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
               ~endorsed=state^.endorsements
             )) {
        result :=
          Some((
            state^.systemIssuer,
            ContributionAccepted(
              Contribution.Acceptance.make(
                ~processId=proposal.processId,
                ~data=proposal.data
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
