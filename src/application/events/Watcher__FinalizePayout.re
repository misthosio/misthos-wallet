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
  let (needsFinalizing, signedTxs, systemIssuer, network) =
    log
    |> EventLog.reduce(
         ((broadcast, txs, systemIssuer, network), {event}) =>
           switch (event) {
           | VentureCreated({systemIssuer, network}) => (
               broadcast,
               txs,
               systemIssuer,
               network,
             )
           | PayoutSigned({processId, payoutTx})
               when ProcessId.eq(processId, payoutProcess) => (
               broadcast,
               [payoutTx, ...txs],
               systemIssuer,
               network,
             )
           | PayoutFinalized({processId})
               when ProcessId.eq(processId, payoutProcess) => (
               false,
               txs,
               systemIssuer,
               network,
             )
           | _ => (broadcast, txs, systemIssuer, network)
           },
         (true, [payoutTx], Bitcoin.ECPair.makeRandom(), Network.Regtest),
       );
  let process = {
    val finalTransaction =
      ref(
        needsFinalizing ?
          Some(PayoutTransaction.finalize(signedTxs, network)) : None,
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
