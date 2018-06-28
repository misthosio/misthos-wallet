open Event;

open PrimitiveTypes;

let make =
    (
      {
        processId: payoutProcess,
        data: {
          payoutTx:
            {usedInputs, misthosFeeAddress, changeAddress} as payoutTx,
        },
      }: Payout.Accepted.t,
      log,
    ) => {
  let (needsFinalizing, signedTxs, systemIssuer) =
    log
    |> EventLog.reduce(
         ((broadcast, txs, systemIssuer), {event}) =>
           switch (event) {
           | VentureCreated({systemIssuer}) => (broadcast, txs, systemIssuer)
           | PayoutSigned({processId, payoutTx})
               when ProcessId.eq(processId, payoutProcess) => (
               broadcast,
               [payoutTx, ...txs],
               systemIssuer,
             )
           | PayoutFinalized({processId})
               when ProcessId.eq(processId, payoutProcess) => (
               false,
               txs,
               systemIssuer,
             )
           | _ => (broadcast, txs, systemIssuer)
           },
         (true, [payoutTx], Bitcoin.ECPair.makeRandom()),
       );
  let process = {
    val finalTransaction =
      ref(
        needsFinalizing ? Some(PayoutTransaction.finalize(signedTxs)) : None,
      );
    val delivered = ref(false);
    pub receive = ({event}: EventLog.item) => {
      let _ignoreThisWarning = this;
      switch (event) {
      | PayoutFinalized({processId})
          when ProcessId.eq(processId, payoutProcess) =>
        finalTransaction := None
      | _ => ()
      };
    };
    pub processCompleted = () =>
      delivered^ || finalTransaction^ |> Js.Option.isNone;
    pub pendingEvent = () =>
      finalTransaction^
      |> Utils.mapOption((tx: Bitcoin.Transaction.t) =>
           (
             systemIssuer,
             PayoutFinalized(
               Event.Payout.Finalized.make(
                 ~processId=payoutProcess,
                 ~payoutTx={
                   txHex: tx |> Bitcoin.Transaction.toHex,
                   usedInputs,
                   misthosFeeAddress,
                   changeAddress,
                 },
                 ~txId=tx |> Bitcoin.Transaction.getId,
               ),
             ),
           )
         )
  };
  process;
};
