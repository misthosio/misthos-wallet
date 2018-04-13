open Event;

open PrimitiveTypes;

let make = ({processId: payoutProcess, data}: Payout.Accepted.t, log) => {
  let (needsBroadcast, signedTxs, systemIssuer, network) =
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
           | PayoutBroadcast({processId})
               when ProcessId.eq(processId, payoutProcess) => (
               false,
               txs,
               systemIssuer,
               network,
             )
           | PayoutBroadcastFailed({processId})
               when ProcessId.eq(processId, payoutProcess) => (
               false,
               txs,
               systemIssuer,
               network,
             )
           | _ => (broadcast, txs, systemIssuer, network)
           },
         (
           true,
           [data.payoutTx],
           Bitcoin.ECPair.makeRandom(),
           Network.Regtest,
         ),
       );
  let process = {
    val finalTransaction =
      ref(
        needsBroadcast ?
          Some(PayoutTransaction.finalize(signedTxs, network)) : None,
      );
    pub receive = ({event}: EventLog.item) =>
      switch (event) {
      | PayoutBroadcast({processId})
          when ProcessId.eq(processId, payoutProcess) =>
        finalTransaction := None
      | PayoutBroadcastFailed({processId})
          when ProcessId.eq(processId, payoutProcess) =>
        finalTransaction := None
      | _ => ()
      };
    pub processCompleted = () => finalTransaction^ |> Js.Option.isNone;
    pub pendingEvent = () =>
      finalTransaction^
      |> Utils.mapOption(tx =>
           Js.Promise.(
             tx
             |> Network.broadcastTransaction(network)
             |> then_(result =>
                  (
                    switch (result) {
                    | WalletTypes.Ok(transactionId) => (
                        systemIssuer,
                        PayoutBroadcast(
                          Payout.Broadcast.make(
                            ~processId=payoutProcess,
                            ~transactionId,
                          ),
                        ),
                      )
                    | WalletTypes.Error(errorMessage) =>
                      Utils.printError(
                        "Broadcasting transaction failed",
                        errorMessage,
                      );
                      (
                        systemIssuer,
                        PayoutBroadcastFailed(
                          Payout.BroadcastFailure.make(
                            ~processId=payoutProcess,
                            ~errorMessage,
                          ),
                        ),
                      );
                    }
                  )
                  |> resolve
                )
           )
         )
  };
  process;
};
