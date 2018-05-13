open Event;

open PrimitiveTypes;

type state = {
  dependencyMet: bool,
  eligable: list(userId),
  endorsements: list(userId),
  policy: Policy.t,
  systemIssuer: Bitcoin.ECPair.t,
};

let make = (proposal: Custodian.Removal.Proposed.t, log) => {
  let process = {
    val state =
      ref({
        dependencyMet: false,
        eligable: [],
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
              eligable: [data.id, ...state^.eligable],
            }
          | PartnerRemovalAccepted({data: {id}}) => {
              ...state^,
              eligable: state^.eligable |> List.filter(UserId.neq(id)),
            }
          | CustodianAccepted({processId})
              when ProcessId.eq(processId, proposal.data.lastCustodianProcess) => {
              ...state^,
              dependencyMet: true,
            }
          | CustodianRemovalEndorsed(event)
              when ProcessId.eq(event.processId, proposal.processId) => {
              ...state^,
              endorsements: [event.supporterId, ...state^.endorsements],
            }
          | CustodianRemovalAccepted(event)
              when ProcessId.eq(event.processId, proposal.processId) =>
            completed := true;
            state^;
          | _ => state^
          }
        );
      result := None;
      if (completed^ == false
          && state^.dependencyMet == true
          && state^.policy
          |> Policy.fulfilled(
               ~eligable=state^.eligable,
               ~endorsed=state^.endorsements,
             )) {
        result :=
          Some((
            state^.systemIssuer,
            CustodianRemovalAccepted(
              Custodian.Removal.Accepted.fromProposal(proposal),
            ),
          ));
      };
    };
    pub processCompleted = () => completed^;
    pub pendingEvent = () => result^ |> Utils.mapOption(Js.Promise.resolve)
  };
  log |> EventLog.reduce((_, item) => process#receive(item), ());
  process;
};
