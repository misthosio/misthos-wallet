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
           | PayoutBroadcastDuplicate({processId})
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
    val delivered = ref(false);
    pub receive = ({event}: EventLog.item) => {
      let _ignoreThisWarning = this;
      switch (event) {
      | PayoutBroadcast({processId})
          when ProcessId.eq(processId, payoutProcess) =>
        finalTransaction := None
      | PayoutBroadcastFailed({processId})
          when ProcessId.eq(processId, payoutProcess) =>
        finalTransaction := None
      | _ => ()
      };
    };
    pub processCompleted = () =>
      delivered^ || finalTransaction^ |> Js.Option.isNone;
    pub pendingEvent = () =>
      finalTransaction^
      |> Utils.mapOption(tx =>
           Js.Promise.(
             tx
             |> Network.broadcastTransaction(network)
             |> then_(result => {
                  delivered := true;
                  (
                    switch (result) {
                    | WalletTypes.Ok(txId) => (
                        systemIssuer,
                        PayoutBroadcast(
                          Payout.Broadcast.make(
                            ~processId=payoutProcess,
                            ~txId,
                          ),
                        ),
                      )
                    | WalletTypes.AlreadyInBlockchain => (
                        systemIssuer,
                        PayoutBroadcastDuplicate(
                          Payout.BroadcastDuplicate.make(
                            ~processId=payoutProcess,
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
                          Payout.BroadcastFailed.make(
                            ~processId=payoutProcess,
                            ~errorMessage,
                          ),
                        ),
                      );
                    | WalletTypes.FetchError(_error) => (
                        systemIssuer,
                        PayoutBroadcastFailed(
                          Payout.BroadcastFailed.make(
                            ~processId=payoutProcess,
                            ~errorMessage="Fetch error",
                          ),
                        ),
                      )
                    }
                  )
                  |> resolve;
                })
           )
         )
  };
  process;
};
