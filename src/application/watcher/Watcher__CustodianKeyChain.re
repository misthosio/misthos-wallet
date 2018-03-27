open Event;

open PrimitiveTypes;

type state = {
  ventureId,
  pendingEvent: option((Bitcoin.ECPair.t, Event.t)),
  nextKeyChainIndex: int
};

let make =
    (
      {userId, appKeyPair, masterKeyChain}: Session.Data.t,
      {data}: Custodian.Acceptance.t,
      log
    ) => {
  let custodianId = data.partnerId;
  let accountIndex = data.accountIndex;
  let process = {
    val state =
      ref({
        ventureId: VentureId.fromString(""),
        pendingEvent: None,
        nextKeyChainIndex: 0
      });
    pub receive = ({event}: EventLog.item) =>
      state :=
        (
          switch event {
          | VentureCreated({ventureId}) => {...state^, ventureId}
          | AccountCreationAccepted(acceptance)
              when acceptance.data.accountIndex == accountIndex => {
              ...state^,
              pendingEvent:
                Some((
                  appKeyPair,
                  CustodianKeyChainUpdated(
                    CustodianKeyChainUpdated.make(
                      ~partnerId=custodianId,
                      ~keyChain=
                        CustodianKeyChain.make(
                          ~ventureId=state^.ventureId,
                          ~accountIndex,
                          ~keyChainIndex=state^.nextKeyChainIndex,
                          ~masterKeyChain
                        )
                        |> CustodianKeyChain.toPublicKeyChain
                    )
                  )
                ))
            }
          | CustodianKeyChainUpdated({partnerId, keyChain})
              when
                UserId.eq(partnerId, custodianId)
                && CustodianKeyChain.accountIndex(keyChain) == accountIndex => {
              ...state^,
              pendingEvent: None,
              nextKeyChainIndex: state^.nextKeyChainIndex + 1
            }
          | _ => state^
          }
        );
    pub processCompleted = () => userId != data.partnerId;
    pub pendingEvent = () => state^.pendingEvent
  };
  log |> EventLog.reduce((_, item) => process#receive(item), ());
  process;
};
