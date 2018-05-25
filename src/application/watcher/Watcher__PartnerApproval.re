open Belt;

open Event;

open PrimitiveTypes;

type state = {
  eligibilityCollector: EligibilityCollector.t,
  endorsements: UserId.set,
  rejections: UserId.set,
  policy: Policy.t,
  systemIssuer: Bitcoin.ECPair.t,
  creatorId: userId,
  received: int,
};

let make = (proposal: Partner.Proposed.t, log) => {
  let process = {
    val state =
      ref({
        eligibilityCollector:
          EligibilityCollector.make(proposal.eligibleWhenProposing),
        endorsements: UserId.emptySet,
        rejections: UserId.emptySet,
        policy: proposal.policy,
        systemIssuer: Bitcoin.ECPair.makeRandom(),
        creatorId: UserId.fromString(""),
        received: 0,
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
          received: state^.received + 1,
        };
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
              endorsements: state^.endorsements |. Set.add(event.supporterId),
            }
          | PartnerRejected(event)
              when ProcessId.eq(event.processId, proposal.processId) => {
              ...state^,
              rejections: state^.rejections |. Set.add(event.rejectorId),
            }
          | PartnerAccepted(event)
              when ProcessId.eq(event.processId, proposal.processId) =>
            completed := true;
            state^;
          | PartnerDenied(event)
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
              PartnerDenied(Partner.Denied.fromProposal(proposal)),
            ))
          | (_, _, true) =>
            Some((
              state^.systemIssuer,
              PartnerAccepted(Partner.Accepted.fromProposal(proposal)),
            ))
          | _ => None
          }
        );
      if (proposal.data.id == state^.creatorId && state^.received == 3) {
        result :=
          Some((
            state^.systemIssuer,
            PartnerAccepted(Partner.Accepted.fromProposal(proposal)),
          ));
      };
    };
    pub processCompleted = () => completed^;
    pub pendingEvent = () => result^
  };
  log |> EventLog.reduce((_, item) => process#receive(item), ());
  process;
};
