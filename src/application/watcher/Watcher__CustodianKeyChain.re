open Event;

open PrimitiveTypes;

open WalletTypes;

type state = {
  ventureId,
  pendingEvent: option((Bitcoin.ECPair.t, Event.t)),
  nextKeyChainIdx: custodianKeyChainIdx
};

let make =
    (
      {userId, issuerKeyPair, masterKeyChain}: Session.Data.t,
      {data}: Custodian.Acceptance.t,
      log
    ) => {
  let custodianId = data.partnerId;
  let accountIdx = data.accountIdx;
  let process = {
    val state =
      ref({
        ventureId: VentureId.fromString(""),
        pendingEvent: None,
        nextKeyChainIdx: CustodianKeyChainIndex.first
      });
    pub receive = ({event}: EventLog.item) =>
      state :=
        (
          switch event {
          | VentureCreated({ventureId}) => {...state^, ventureId}
          | AccountCreationAccepted(acceptance)
              when acceptance.data.accountIdx == accountIdx => {
              ...state^,
              pendingEvent:
                Some((
                  issuerKeyPair,
                  CustodianKeyChainUpdated(
                    CustodianKeyChainUpdated.make(
                      ~partnerId=custodianId,
                      ~keyChain=
                        CustodianKeyChain.make(
                          ~ventureId=state^.ventureId,
                          ~accountIdx,
                          ~keyChainIdx=state^.nextKeyChainIdx,
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
                && CustodianKeyChain.accountIdx(keyChain) == accountIdx => {
              ...state^,
              pendingEvent: None,
              nextKeyChainIdx:
                state^.nextKeyChainIdx |> CustodianKeyChainIndex.next
            }
          | _ => state^
          }
        );
    pub processCompleted = () => userId != data.partnerId;
    pub pendingEvent = () =>
      state^.pendingEvent |> Utils.mapOption(Js.Promise.resolve)
  };
  log |> EventLog.reduce((_, item) => process#receive(item), ());
  process;
};
