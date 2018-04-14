open Event;

open PrimitiveTypes;

open WalletTypes;

type state = {
  network: Network.t,
  ventureId,
  accountKeyChains:
    list((accountIdx, list((accountKeyChainIdx, AccountKeyChain.t)))),
  payoutTx: option(PayoutTransaction.t),
};

let make =
    (
      {userId, issuerKeyPair, masterKeyChain}: Session.Data.t,
      {processId: payoutProcess, supporterId}: Payout.Endorsed.t,
      log,
    ) => {
  let state =
    log
    |> EventLog.reduce(
         (state, {event}) =>
           switch (event) {
           | VentureCreated({ventureId, network}) => {
               ...state,
               ventureId,
               network,
             }
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
                   [(keyChain.keyChainIdx, keyChain), ...accountKeyChains],
                 ),
                 ...state.accountKeyChains
                    |> List.remove_assoc(keyChain.accountIdx),
               ],
             };
           | PayoutProposed({processId, data})
               when ProcessId.eq(processId, payoutProcess) => {
               ...state,
               payoutTx: Some(data.payoutTx),
             }
           | _ => state
           },
         {
           network: Network.Regtest,
           ventureId: VentureId.fromString(""),
           accountKeyChains: [],
           payoutTx: None,
         },
       );
  let signEvent =
    if (UserId.eq(supporterId, userId)) {
      switch (
        PayoutTransaction.signPayout(
          ~ventureId=state.ventureId,
          ~userId,
          ~masterKeyChain,
          ~accountKeyChains=state.accountKeyChains,
          ~payoutTx=state.payoutTx |> Js.Option.getExn,
          ~network=state.network,
        )
      ) {
      | Signed(payoutTx) =>
        Some((
          issuerKeyPair,
          PayoutSigned(
            Payout.Signature.make(
              ~processId=payoutProcess,
              ~custodianId=userId,
              ~payoutTx,
            ),
          ),
        ))
      | NotSigned => None
      };
    } else {
      None;
    };
  let process = {
    val signPending =
      ref(
        switch (signEvent) {
        | Some(_) => true
        | None => false
        },
      );
    pub receive = ({event}: EventLog.item) => {
      let _ignoreThisWarning = this;
      switch (event) {
      | PayoutSigned({custodianId, processId: signingProcess})
          when
            UserId.eq(custodianId, userId)
            && ProcessId.eq(signingProcess, payoutProcess) =>
        signPending := false
      | _ => ()
      };
    };
    pub processCompleted = () => signPending^;
    pub pendingEvent = () =>
      signPending^ ? signEvent |> Utils.mapOption(Js.Promise.resolve) : None
  };
  process;
};
