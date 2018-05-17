open Event;

open PrimitiveTypes;

type state = {
  eligible: list(userId),
  endorsements: list(userId),
  policy: Policy.t,
  systemIssuer: Bitcoin.ECPair.t,
  creatorId: userId,
};

let make = (proposal: Partner.Proposed.t, log) => {
  let process = {
    val state =
      ref({
        eligible: [],
        endorsements: [proposal.supporterId],
        policy: proposal.policy,
        systemIssuer: Bitcoin.ECPair.makeRandom(),
        creatorId: UserId.fromString(""),
      });
    val completed = ref(false);
    val result = ref(None);
    pub receive = ({event}: EventLog.item) => {
      let _ignoreThisWarning = this;
      state :=
        (
          switch (event) {
          | VentureCreated(event) => {
              ...state^,
              systemIssuer: event.systemIssuer,
              creatorId: event.creatorId,
            }
          | PartnerEndorsed(event)
              when ProcessId.eq(event.processId, proposal.processId) => {
              ...state^,
              endorsements: [event.supporterId, ...state^.endorsements],
            }
          | PartnerAccepted(event)
              when ProcessId.eq(event.processId, proposal.processId) =>
            completed := true;
            state^;
          | PartnerAccepted({data}) => {
              ...state^,
              eligible: [data.id, ...state^.eligible],
            }
          | PartnerRemovalAccepted({data: {id}}) => {
              ...state^,
              eligible: state^.eligible |> List.filter(UserId.neq(id)),
              endorsements:
                state^.endorsements |> List.filter(UserId.neq(id)),
            }
          | _ => state^
          }
        );
      result := None;
      if (completed^ == false
          && state^.policy
          |> Policy.fulfilled(
               ~eligible=state^.eligible,
               ~endorsed=state^.endorsements,
             )) {
        result :=
          Some((
            state^.systemIssuer,
            PartnerAccepted(Partner.Accepted.fromProposal(proposal)),
          ));
      };
      if (proposal.data.id == state^.creatorId && log |> EventLog.length == 2) {
        result :=
          Some((
            state^.systemIssuer,
            PartnerAccepted(Partner.Accepted.fromProposal(proposal)),
          ));
      };
    };
    pub processCompleted = () => completed^;
    pub pendingEvent = () => result^ |> Utils.mapOption(Js.Promise.resolve)
  };
  log |> EventLog.reduce((_, item) => process#receive(item), ());
  process;
};
