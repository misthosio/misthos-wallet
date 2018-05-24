open Event;

open PrimitiveTypes;

let make =
    ({payoutTx: {txHex}, processId: payoutProcess}: Payout.Finalized.t, log) => {
  let (needsBroadcast, systemIssuer, network) =
    log
    |> EventLog.reduce(
         ((broadcast, systemIssuer, network), {event}) =>
           switch (event) {
           | VentureCreated({systemIssuer, network}) => (
               broadcast,
               systemIssuer,
               network,
             )
           | PayoutBroadcast({processId})
               when ProcessId.eq(processId, payoutProcess) => (
               false,
               systemIssuer,
               network,
             )
           | PayoutBroadcastDuplicate({processId})
               when ProcessId.eq(processId, payoutProcess) => (
               false,
               systemIssuer,
               network,
             )
           | PayoutBroadcastFailed({processId})
               when ProcessId.eq(processId, payoutProcess) => (
               false,
               systemIssuer,
               network,
             )
           | _ => (broadcast, systemIssuer, network)
           },
         (true, Bitcoin.ECPair.makeRandom(), Network.Regtest),
       );
  let process = {
    val finalTransaction =
      ref(
        needsBroadcast ? Some(txHex |> Bitcoin.Transaction.fromHex) : None,
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
