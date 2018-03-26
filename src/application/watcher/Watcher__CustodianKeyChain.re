open Event;

open PrimitiveTypes;

type state = {
  ventureId,
  pendingEvents: list(CustodianKeyChainUpdated.t),
  currentkeyChains: list(list((int, CustodianKeyChain.t)))
};

let make =
    (
      {userId, masterKeyChain}: Session.Data.t,
      {data}: Custodian.Acceptance.t,
      log
    ) => {
  let custodianId = data.partnerId;
  let accountIndex = data.accountIndex;
  let process = {
    val state =
      ref({
        ventureId: VentureId.fromString(""),
        pendingEvents: [],
        currentkeyChains: []
      });
    val result = ref(None);
    pub receive = ({event}: EventLog.item) => {
      state :=
        (
          switch event {
          | VentureCreated({ventureId}) => {...state^, ventureId}
          | AccountCreationAccepted(acceptance)
              when acceptance.data.accountIndex == accountIndex => {
              ...state^,
              pendingEvents: [
                CustodianKeyChainUpdated.make(
                  ~partnerId=custodianId,
                  ~keyChain=
                    CustodianKeyChain.make(
                      ~ventureId=state^.ventureId,
                      ~accountIndex,
                      ~keyChainIndex=0,
                      ~masterKeyChain
                    )
                    |> CustodianKeyChain.toPublicKeyChain
                )
              ]
            }
          | _ => state^
          }
        );
      result :=
        (
          switch state^ {
          | _ => None
          }
        );
    };
    pub processCompleted = () => userId != data.partnerId;
    pub pendingEvent = () => result^
  };
  log |> EventLog.reduce((_, item) => process#receive(item), ());
  process;
};
