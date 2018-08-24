open Event;

open PrimitiveTypes;

type state = {
  pendingEvent: option((Bitcoin.ECPair.t, Event.t)),
  completed: bool,
};

let make =
    (
      {userId, issuerKeyPair}: SessionData.t,
      {
        processId: custodianProcessId,
        data: {partnerId, partnerApprovalProcess},
      }: Custodian.Proposed.t,
      log,
    ) => {
  let process = {
    val state =
      ref({
        pendingEvent:
          UserId.eq(partnerId, userId) ?
            Some((
              issuerKeyPair,
              Event.makeCustodianEndorsed(
                ~processId=custodianProcessId,
                ~supporterId=userId,
              ),
            )) :
            None,
        completed: false,
      });
    pub receive = ({event}: EventLog.item) => {
      let _ignoreThisWarning = this;
      state :=
        (
          switch (event, UserId.eq(partnerId, userId)) {
          | (_, false) => {pendingEvent: None, completed: true}
          /* Creators suggests themselves */
          | (CustodianProposed({proposerId}), true)
              when UserId.eq(proposerId, userId) => {
              ...state^,
              completed: true,
            }
          | (CustodianEndorsed({processId, supporterId}), true)
              when
                ProcessId.eq(processId, custodianProcessId)
                && UserId.eq(supporterId, userId) => {
              pendingEvent: None,
              completed: true,
            }
          | (CustodianAccepted({processId}), true)
              when ProcessId.eq(processId, custodianProcessId) => {
              pendingEvent: None,
              completed: true,
            }
          | (PartnerRemovalAccepted({data: {lastPartnerProcess}}), true)
              when ProcessId.eq(lastPartnerProcess, partnerApprovalProcess) => {
              pendingEvent: None,
              completed: true,
            }
          | _ => state^
          }
        );
    };
    pub processCompleted = () => state^.completed;
    pub pendingEvent = () => state^.pendingEvent
  };
  log |> EventLog.reduce((_, item) => process#receive(item), ());
  process;
};
