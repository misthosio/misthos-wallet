open Event;

open PrimitiveTypes;

type state = {
  eligable: list(userId),
  endorsements: list(userId),
  policy: Policy.t,
  systemIssuer: Bitcoin.ECPair.t,
  creatorId: userId,
};

let make = (proposal: Partner.Proposed.t, log) => {
  let process = {
    val state =
      ref({
        eligable: [],
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
              eligable: [data.id, ...state^.eligable],
            }
          | PartnerRemovalAccepted({data: {id}}) => {
              ...state^,
              eligable: state^.eligable |> List.filter(UserId.neq(id)),
            }
          | _ => state^
          }
        );
      result := None;
      if (completed^ == false
          && state^.policy
          |> Policy.fulfilled(
               ~eligable=state^.eligable,
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
