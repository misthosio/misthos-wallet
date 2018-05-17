open Event;

open PrimitiveTypes;

type state = {
  eligible: list(userId),
  endorsements: list(userId),
  policy: Policy.t,
  systemIssuer: Bitcoin.ECPair.t,
};

let make = (proposal: Custodian.Proposed.t, log) => {
  let process = {
    val state =
      ref({
        eligible: [],
        endorsements: [proposal.supporterId],
        policy: proposal.policy,
        systemIssuer: Bitcoin.ECPair.makeRandom(),
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
            }
          | PartnerAccepted({data}) => {
              ...state^,
              eligible: [data.id, ...state^.eligible],
            }
          | PartnerRemovalAccepted({data: {lastPartnerProcess}})
              when
                ProcessId.eq(
                  lastPartnerProcess,
                  proposal.data.partnerApprovalProcess,
                ) =>
            completed := true;
            state^;
          | PartnerRemovalAccepted({data: {id}}) => {
              ...state^,
              eligible: state^.eligible |> List.filter(UserId.neq(id)),
            }
          | CustodianEndorsed(event)
              when ProcessId.eq(event.processId, proposal.processId) => {
              ...state^,
              endorsements: [event.supporterId, ...state^.endorsements],
            }
          | CustodianAccepted(event)
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
               ~eligible=state^.eligible,
               ~endorsed=state^.endorsements,
             )) {
        result :=
          Some((
            state^.systemIssuer,
            CustodianAccepted(Custodian.Accepted.fromProposal(proposal)),
          ));
      };
    };
    pub processCompleted = () => completed^;
    pub pendingEvent = () => result^ |> Utils.mapOption(Js.Promise.resolve)
  };
  log |> EventLog.reduce((_, item) => process#receive(item), ());
  process;
};
