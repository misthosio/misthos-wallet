open Event;

open PrimitiveTypes;

open WalletTypes;

type state = {
  ventureId,
  accountKeyChains:
    list((accountIdx, list((accountKeyChainIdx, AccountKeyChain.t)))),
  payoutTx: option(PayoutTransaction.t)
};

let make =
    (
      {userId, issuerKeyPair, masterKeyChain}: Session.Data.t,
      {processId as payoutProcess, supporterId}: Payout.Endorsement.t,
      log
    ) => {
  let state =
    log
    |> EventLog.reduce(
         (state, {event}) =>
           switch event {
           | VentureCreated({ventureId}) => {...state, ventureId}
           | AccountKeyChainUpdated(({keyChain}: AccountKeyChainUpdated.t)) =>
             let accountKeyChains =
               try (state.accountKeyChains |> List.assoc(keyChain.accountIdx)) {
               | Not_found => []
               };
             {
               ...state,
               accountKeyChains: [
                 (
                   keyChain.accountIdx,
                   [(keyChain.keyChainIdx, keyChain), ...accountKeyChains]
                 ),
                 ...state.accountKeyChains
                    |> List.remove_assoc(keyChain.accountIdx)
               ]
             };
           | PayoutProposed({processId as proposalProcess, data})
               when ProcessId.eq(proposalProcess, payoutProcess) => {
               ...state,
               payoutTx: Some(data.payoutTx)
             }
           | _ => state
           },
         {
           ventureId: VentureId.fromString(""),
           accountKeyChains: [],
           payoutTx: None
         }
       );
  let signEvent =
    switch (
      PayoutTransaction.signPayout(
        ~ventureId=state.ventureId,
        ~userId,
        ~masterKeyChain,
        ~accountKeyChains=state.accountKeyChains,
        ~payoutTx=state.payoutTx |> Js.Option.getExn,
        ~network=Network.Regtest.network
      )
    ) {
    | Signed(payoutTx) =>
      Some((
        issuerKeyPair,
        PayoutSigned(
          Payout.Signature.make(
            ~processId=payoutProcess,
            ~custodianId=userId,
            ~payoutTx
          )
        )
      ))
    | NotSigned => None
    };
  let process = {
    val signPending =
      ref(
        switch signEvent {
        | Some(_) => true
        | None => false
        }
      );
    pub receive = ({event}: EventLog.item) =>
      switch event {
      | PayoutSigned({custodianId, processId as signingProcess})
          when
            UserId.eq(custodianId, userId)
            && ProcessId.eq(signingProcess, payoutProcess) =>
        signPending := false
      | _ => ()
      };
    pub processCompleted = () => signPending^;
    pub pendingEvent = () =>
      signPending^ ? signEvent |> Utils.mapOption(Js.Promise.resolve) : None
  };
  process;
};
