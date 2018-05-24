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
};

let make = (proposal: Partner.Removal.Proposed.t, log) => {
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
              creatorId: event.creatorId,
            }
          | PartnerRemovalEndorsed(event)
              when ProcessId.eq(event.processId, proposal.processId) => {
              ...state^,
              endorsements: state^.endorsements |. Set.add(event.supporterId),
            }
          | PartnerRemovalRejected(event)
              when ProcessId.eq(event.processId, proposal.processId) => {
              ...state^,
              rejections: state^.rejections |. Set.add(event.rejectorId),
            }
          | PartnerRemovalAccepted(event)
              when ProcessId.eq(event.processId, proposal.processId) =>
            completed := true;
            state^;
          | PartnerRemovalDenied(event)
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
              PartnerRemovalDenied(
                Partner.Removal.Denied.fromProposal(proposal),
              ),
            ))
          | (_, _, true) =>
            Some((
              state^.systemIssuer,
              PartnerRemovalAccepted(
                Partner.Removal.Accepted.fromProposal(proposal),
              ),
            ))
          | _ => None
          }
        );
    };
    pub processCompleted = () => completed^;
    pub pendingEvent = () => result^ |> Utils.mapOption(Js.Promise.resolve)
  };
  log |> EventLog.reduce((_, item) => process#receive(item), ());
  process;
};
