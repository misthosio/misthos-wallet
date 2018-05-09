open Event;

open PrimitiveTypes;

open WalletTypes;

type state = {
  ventureId,
  pendingEvent: option((Bitcoin.ECPair.t, Event.t)),
  selfRemoved: bool,
  nextKeyChainIdx: custodianKeyChainIdx,
};

let make =
    (
      {userId, issuerKeyPair, masterKeyChain}: Session.Data.t,
      {data, processId: custodianApprovalProcess}: Custodian.Accepted.t,
      log,
    ) => {
  let custodianId = data.partnerId;
  let accountIdx = data.accountIdx;
  let process = {
    val state =
      ref({
        ventureId: VentureId.fromString(""),
        pendingEvent: None,
        selfRemoved: false,
        nextKeyChainIdx: CustodianKeyChainIndex.first,
      });
    pub receive = ({event}: EventLog.item) => {
      let _ignoreThisWarning = this;
      state :=
        (
          switch (event) {
          | VentureCreated({ventureId}) => {...state^, ventureId}
          | AccountCreationAccepted(acceptance)
              when acceptance.data.accountIdx == accountIdx => {
              ...state^,
              pendingEvent:
                Some((
                  issuerKeyPair,
                  CustodianKeyChainUpdated(
                    CustodianKeyChainUpdated.make(
                      ~custodianApprovalProcess,
                      ~custodianId,
                      ~keyChain=
                        CustodianKeyChain.make(
                          ~ventureId=state^.ventureId,
                          ~accountIdx,
                          ~keyChainIdx=state^.nextKeyChainIdx,
                          ~masterKeyChain,
                        )
                        |> CustodianKeyChain.toPublicKeyChain,
                    ),
                  ),
                )),
            }
          | CustodianRemovalAccepted({
              dependsOnCompletions,
              data: {custodianId: removedId, accountIdx: fromAccount},
            })
              when
                UserId.eq(removedId, custodianId)
                && ProcessId.eq(
                     dependsOnCompletions |> List.hd,
                     custodianApprovalProcess,
                   )
                && AccountIndex.eq(fromAccount, accountIdx) => {
              ...state^,
              selfRemoved: true,
            }
          | CustodianRemovalAccepted({
              data: {custodianId: removedId, accountIdx: fromAccount},
            })
              when
                UserId.neq(removedId, custodianId)
                && AccountIndex.eq(fromAccount, accountIdx) => {
              ...state^,
              pendingEvent:
                Some((
                  issuerKeyPair,
                  CustodianKeyChainUpdated(
                    CustodianKeyChainUpdated.make(
                      ~custodianApprovalProcess,
                      ~custodianId,
                      ~keyChain=
                        CustodianKeyChain.make(
                          ~ventureId=state^.ventureId,
                          ~accountIdx,
                          ~keyChainIdx=state^.nextKeyChainIdx,
                          ~masterKeyChain,
                        )
                        |> CustodianKeyChain.toPublicKeyChain,
                    ),
                  ),
                )),
            }
          | CustodianKeyChainUpdated({
              custodianApprovalProcess: processId,
              custodianId: custodian,
              keyChain,
            })
              when
                UserId.eq(custodian, custodianId)
                && ProcessId.eq(custodianApprovalProcess, processId)
                && CustodianKeyChain.accountIdx(keyChain) == accountIdx => {
              ...state^,
              pendingEvent: None,
              nextKeyChainIdx:
                state^.nextKeyChainIdx |> CustodianKeyChainIndex.next,
            }
          | CustodianKeyChainUpdated({custodianId: custodian, keyChain})
              when
                UserId.eq(custodian, custodianId)
                && CustodianKeyChain.accountIdx(keyChain) == accountIdx => {
              ...state^,
              pendingEvent:
                state^.pendingEvent |> Utils.mapOption(_=>((
                  issuerKeyPair,
                  CustodianKeyChainUpdated(
                    CustodianKeyChainUpdated.make(
                      ~custodianApprovalProcess,
                      ~custodianId,
                      ~keyChain=
                        CustodianKeyChain.make(
                          ~ventureId=state^.ventureId,
                          ~accountIdx,
                          ~keyChainIdx=state^.nextKeyChainIdx |> CustodianKeyChainIndex.next,
                          ~masterKeyChain,
                        )
                        |> CustodianKeyChain.toPublicKeyChain,
                    ),
                  ),
                ))),
              nextKeyChainIdx:
                state^.nextKeyChainIdx |> CustodianKeyChainIndex.next,
            }
          | _ => state^
          }
        );
    };
    pub processCompleted = () =>
      userId != data.partnerId || state^.selfRemoved;
    pub pendingEvent = () =>
      state^.pendingEvent |> Utils.mapOption(Js.Promise.resolve)
  };
  log |> EventLog.reduce((_, item) => process#receive(item), ());
  process;
};
