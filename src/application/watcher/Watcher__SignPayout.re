open Event;

open PrimitiveTypes;

type state = {
  network: Network.t,
  ventureId,
  accountKeyChains: AccountKeyChain.Collection.t,
  payoutTx: option(PayoutTransaction.t),
  complete: bool,
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
           | AccountKeyChainIdentified(
               ({keyChain}: AccountKeyChainIdentified.t),
             ) => {
               ...state,
               accountKeyChains:
                 state.accountKeyChains
                 |> AccountKeyChain.Collection.add(keyChain),
             }
           | PayoutProposed({processId, data})
               when ProcessId.eq(processId, payoutProcess) => {
               ...state,
               payoutTx: Some(data.payoutTx),
             }
           | PayoutSigned({custodianId, processId: signingProcess})
               when
                 UserId.eq(custodianId, userId)
                 && ProcessId.eq(signingProcess, payoutProcess) => {
               ...state,
               complete: true,
             }
           | _ => state
           },
         {
           network: Network.Regtest,
           ventureId: VentureId.fromString(""),
           accountKeyChains: AccountKeyChain.Collection.empty,
           payoutTx: None,
           complete: false,
         },
       );
  let signEvent =
    if (UserId.eq(supporterId, userId) && state.complete == false) {
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
            Payout.Signed.make(
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
    pub processCompleted = () => signPending^ == false;
    pub pendingEvent = () =>
      signPending^ ? signEvent |> Utils.mapOption(Js.Promise.resolve) : None
  };
  process;
};
