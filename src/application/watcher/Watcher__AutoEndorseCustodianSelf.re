open Event;

open PrimitiveTypes;

type state = {
  pendingEvent: option((Bitcoin.ECPair.t, Event.t)),
  custodianProcessId: processId,
  completed: bool,
};

let make =
    (
      {userId, issuerKeyPair}: Session.Data.t,
      {processId: partnerApprovalProcess, data: partnerData}: Partner.Accepted.t,
      log,
    ) => {
  let acceptedPartnerId = partnerData.id;
  let process = {
    val state =
      ref({
        pendingEvent: None,
        completed: false,
        custodianProcessId: ProcessId.make(),
      });
    pub receive = ({event}: EventLog.item) => {
      let _ignoreThisWarning = this;
      state :=
        (
          switch (event, UserId.eq(acceptedPartnerId, userId)) {
          | (_, false) => {...state^, pendingEvent: None, completed: true}
          /* Creators suggests themselves */
          | (CustodianProposed({proposerId}), true)
              when UserId.eq(proposerId, userId) => {
              ...state^,
              completed: true,
            }
          | (CustodianProposed({data, processId}), true)
              when
                UserId.eq(data.partnerId, userId)
                && ProcessId.eq(
                     data.partnerApprovalProcess,
                     partnerApprovalProcess,
                   ) => {
              ...state^,
              pendingEvent:
                Some((
                  issuerKeyPair,
                  Event.makeCustodianEndorsed(
                    ~processId,
                    ~supporterId=userId,
                  ),
                )),
              custodianProcessId: processId,
            }
          | (CustodianEndorsed({processId, supporterId}), true)
              when
                ProcessId.eq(processId, state^.custodianProcessId)
                && UserId.eq(supporterId, userId) => {
              ...state^,
              pendingEvent: None,
              completed: true,
            }
          | (CustodianAccepted({processId}), true)
              when ProcessId.eq(processId, state^.custodianProcessId) => {
              ...state^,
              pendingEvent: None,
              completed: true,
            }
          | _ => state^
          }
        );
    };
    pub processCompleted = () => state^.completed;
    pub pendingEvent = () =>
      state^.pendingEvent |> Utils.mapOption(Js.Promise.resolve)
  };
  log |> EventLog.reduce((_, item) => process#receive(item), ());
  process;
};
