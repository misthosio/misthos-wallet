open Belt;

open Event;

open PrimitiveTypes;

type state = {
  eligibilityCollector: EligibilityCollector.t,
  endorsements: UserId.set,
  rejections: UserId.set,
  policy: Policy.t,
  systemIssuer: Bitcoin.ECPair.t,
};

let make = (proposal: Custodian.Proposed.t, log) => {
  let process = {
    val state =
      ref({
        eligibilityCollector:
          EligibilityCollector.make(proposal.eligibleWhenProposing),
        endorsements: UserId.emptySet,
        rejections: UserId.emptySet,
        policy: proposal.policy,
        systemIssuer: Bitcoin.ECPair.makeRandom(),
      });
    val completed = ref(false);
    val result = ref(None);
    pub receive = ({event}: EventLog.item) => {
      let _ignoreThisWarning = this;
      state :=
        {
          ...state^,
          eligibilityCollector:
            state^.eligibilityCollector |> EligibilityCollector.apply(event),
        };
      state :=
        (
          switch (event) {
          | VentureCreated(event) => {
              ...state^,
              systemIssuer: event.systemIssuer,
            }
          | PartnerRemovalAccepted({data: {lastPartnerProcess}})
              when
                ProcessId.eq(
                  lastPartnerProcess,
                  proposal.data.partnerApprovalProcess,
                ) =>
            completed := true;
            state^;
          | CustodianEndorsed(event)
              when ProcessId.eq(event.processId, proposal.processId) => {
              ...state^,
              endorsements: state^.endorsements |. Set.add(event.supporterId),
            }
          | CustodianRejected(event)
              when ProcessId.eq(event.processId, proposal.processId) => {
              ...state^,
              rejections: state^.rejections |. Set.add(event.rejectorId),
            }
          | CustodianAccepted(event)
              when ProcessId.eq(event.processId, proposal.processId) =>
            completed := true;
            state^;
          | CustodianDenied(event)
              when ProcessId.eq(event.processId, proposal.processId) =>
            completed := true;
            state^;
          | _ => state^
          }
        );
      result :=
        (
          switch (
            completed^,
            state^.policy
            |> Policy.canBeFulfilled(
                 ~eligible=
                   state^.eligibilityCollector
                   |> EligibilityCollector.currentEligible,
                 ~rejected=state^.rejections,
               ),
            state^.policy
            |> Policy.fulfilled(
                 ~eligible=
                   state^.eligibilityCollector
                   |> EligibilityCollector.currentEligible,
                 ~endorsed=state^.endorsements,
               ),
          ) {
          | (true, _, _) => None
          | (_, false, _) =>
            Some((
              state^.systemIssuer,
              CustodianDenied(Custodian.Denied.fromProposal(proposal)),
            ))
          | (_, _, true) =>
            Some((
              state^.systemIssuer,
              CustodianAccepted(Custodian.Accepted.fromProposal(proposal)),
            ))
          | _ => None
          }
        );
    };
    pub processCompleted = () => completed^;
    pub pendingEvent = () => result^
  };
  log |> EventLog.reduce((_, item) => process#receive(item), ());
  process;
};
