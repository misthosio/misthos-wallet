open Event;

open PrimitiveTypes;

let make = ({processId: payoutProcess, data}: Payout.Acceptance.t, log) => {
  let (broadcast, signedTxs, systemIssuer) =
    log
    |> EventLog.reduce(
         ((broadcast, txs, systemIssuer), {event}) =>
           switch event {
           | VentureCreated({systemIssuer}) => (broadcast, txs, systemIssuer)
           | PayoutSigned({processId, payoutTx})
               when ProcessId.eq(processId, payoutProcess) => (
               broadcast,
               [payoutTx, ...txs],
               systemIssuer
             )
           | PayoutBroadcast({processId})
               when ProcessId.eq(processId, payoutProcess) => (
               true,
               txs,
               systemIssuer
             )
           | PayoutBroadcastFailed({processId})
               when ProcessId.eq(processId, payoutProcess) => (
               true,
               txs,
               systemIssuer
             )
           | _ => (broadcast, txs, systemIssuer)
           },
         (false, [data.payoutTx], Bitcoin.ECPair.makeRandom())
       );
  let process = {
    val finalTransaction =
      ref(
        broadcast ?
          Some(
            PayoutTransaction.finalize(signedTxs, Network.Regtest.network)
          ) :
          None
      );
    pub receive = ({event}: EventLog.item) =>
      switch event {
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
             |> Network.Regtest.broadcastTransaction
             |> then_(result =>
                  (
                    switch result {
                    | WalletTypes.Ok(transactionId) => (
                        systemIssuer,
                        PayoutBroadcast(
                          Payout.Broadcast.make(
                            ~processId=payoutProcess,
                            ~transactionId
                          )
                        )
                      )
                    | WalletTypes.Error(errorMessage) =>
                      Utils.printError(
                        "Broadcasting transaction failed",
                        errorMessage
                      );
                      (
                        systemIssuer,
                        PayoutBroadcastFailed(
                          Payout.BroadcastFailure.make(
                            ~processId=payoutProcess,
                            ~errorMessage
                          )
                        )
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
