open Event;

open PrimitiveTypes;

open WalletTypes;

type state = {
  ventureId,
  pendingEvent: option(unit => (Bitcoin.ECPair.t, Event.t)),
  selfRemoved: bool,
  pubKeyPresent: bool,
  hardwareIdPresent: bool,
  nextKeyChainIdx: custodianKeyChainIdx,
};

let make =
    (
      {userId, issuerKeyPair, masterKeyChain}: SessionData.t,
      {
        data: {partnerId: custodianId, accountIdx, partnerApprovalProcess},
        processId: custodianApprovalProcess,
      }: Custodian.Accepted.t,
      log,
    ) => {
  let process = {
    val state =
      ref({
        ventureId: VentureId.fromString(""),
        pendingEvent: None,
        selfRemoved: false,
        pubKeyPresent: false,
        hardwareIdPresent: false,
        nextKeyChainIdx: CustodianKeyChainIndex.first,
      });
    pub receive = ({event}: EventLog.item) => {
      let _ignoreThisWarning = this;
      state :=
        (
          switch (event) {
          | VentureCreated({ventureId}) => {...state^, ventureId}
          | PartnerProposed({processId, data: {pubKey}})
              when ProcessId.eq(processId, partnerApprovalProcess) => {
              ...state^,
              pubKeyPresent: pubKey |> Js.Option.isSome,
            }
          | PartnerPubKeyAdded({partnerId}) when UserId.eq(partnerId, userId) => {
              ...state^,
              pubKeyPresent: true,
            }
          | AccountCreationAccepted(acceptance)
              when acceptance.data.accountIdx == accountIdx => {
              ...state^,
              pendingEvent:
                Some(
                  (
                    () => (
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
                    )
                  ),
                ),
            }
          | CustodianRemovalAccepted({
              data: {
                lastCustodianProcess,
                custodianId: removedId,
                accountIdx: fromAccount,
              },
            })
              when
                UserId.eq(removedId, custodianId)
                && ProcessId.eq(
                     lastCustodianProcess,
                     custodianApprovalProcess,
                   )
                && AccountIndex.eq(fromAccount, accountIdx) => {
              ...state^,
              pendingEvent: None,
              selfRemoved: true,
            }
          | PartnerRemovalAccepted({data: {id, lastPartnerProcess}})
              when
                UserId.eq(custodianId, id)
                && ProcessId.eq(lastPartnerProcess, partnerApprovalProcess) => {
              ...state^,
              pendingEvent: None,
              selfRemoved: true,
            }
          | PartnerRemovalAccepted(_) => {
              ...state^,
              pendingEvent:
                Some(
                  (
                    () => (
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
                    )
                  ),
                ),
            }
          | CustodianKeyChainUpdated({
              custodianApprovalProcess: processId,
              custodianId: custodian,
              keyChain,
            })
              when
                UserId.eq(custodian, custodianId)
                && ProcessId.eq(custodianApprovalProcess, processId)
                && AccountIndex.eq(
                     CustodianKeyChain.accountIdx(keyChain),
                     accountIdx,
                   ) => {
              ...state^,
              pendingEvent: None,
              hardwareIdPresent:
                keyChain |> CustodianKeyChain.hardwareId |> Js.Option.isSome,
              nextKeyChainIdx:
                state^.nextKeyChainIdx |> CustodianKeyChainIndex.next,
            }
          | CustodianKeyChainUpdated({custodianId: custodian, keyChain})
              when
                UserId.eq(custodian, custodianId)
                && AccountIndex.eq(
                     CustodianKeyChain.accountIdx(keyChain),
                     accountIdx,
                   ) => {
              ...state^,
              hardwareIdPresent:
                keyChain |> CustodianKeyChain.hardwareId |> Js.Option.isSome,
              pendingEvent:
                state^.pendingEvent
                |> Utils.mapOption((_, ()) =>
                     (
                       issuerKeyPair,
                       CustodianKeyChainUpdated(
                         CustodianKeyChainUpdated.make(
                           ~custodianApprovalProcess,
                           ~custodianId,
                           ~keyChain=
                             CustodianKeyChain.make(
                               ~ventureId=state^.ventureId,
                               ~accountIdx,
                               ~keyChainIdx=
                                 state^.nextKeyChainIdx
                                 |> CustodianKeyChainIndex.next,
                               ~masterKeyChain,
                             )
                             |> CustodianKeyChain.toPublicKeyChain,
                         ),
                       ),
                     )
                   ),
              nextKeyChainIdx:
                state^.nextKeyChainIdx |> CustodianKeyChainIndex.next,
            }
          | _ => state^
          }
        );
    };
    pub processCompleted = () =>
      UserId.neq(userId, custodianId) || state^.selfRemoved;
    pub pendingEvent = () =>
      !state^.hardwareIdPresent && state^.pubKeyPresent ?
        state^.pendingEvent |> Utils.mapOption(f => f()) : None
  };
  if (process#processCompleted() == false) {
    log |> EventLog.reduce((_, item) => process#receive(item), ());
  };
  process;
};
