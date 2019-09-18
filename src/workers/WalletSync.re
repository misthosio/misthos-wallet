[@bs.val] external _postMessage: WebWorker.message => unit = "postMessage";

let postMessage = msg =>
  {"payload": msg |> VentureWorkerMessage.encodeIncoming, "correlationId": ""}
  |> _postMessage;

open Belt;

open PrimitiveTypes;

let logLabel = "[Wallet Sync]";

let logMessage = WorkerUtils.logMessage(logLabel);

let catchAndLogError = WorkerUtils.catchAndLogError(logLabel);

let notifyOfUnlockedInputs =
    (
      ventureId,
      blockHeight,
      {confirmedTransactions}: TransactionCollector.t,
      walletInfo,
    ) =>
  switch (
    (walletInfo |> WalletInfoCollector.allUnspentInputs)
    ->(
        Set.reduceU(
          [], (. res, {txId, sequence, unlocked} as input: Network.txInput) =>
          switch (
            unlocked,
            sequence,
            confirmedTransactions->(Map.String.get(txId)),
          ) {
          | (false, Some(sequence), Some(txBlock))
              when blockHeight > sequence + int_of_float(txBlock) => [
              Event.Income.Unlocked.make(~input={...input, unlocked: true}),
              ...res,
            ]
          | _ => res
          }
        )
      )
  ) {
  | [] => ()
  | events =>
    postMessage(
      VentureWorkerMessage.SyncWallet(ventureId, [], [], [], events, [], []),
    )
  };

let broadcastPayouts =
    ({ventureId, network, notYetBroadcastPayouts}: TransactionCollector.t) =>
  Js.Promise.(
    notYetBroadcastPayouts
    ->(
        Map.forEachU(
          (. processId, {txId, payoutTx: {txHex}}: Event.Payout.Finalized.t) =>
          txHex
          |> Bitcoin.Transaction.fromHex
          |> NetworkClient.broadcastTransaction(network)
          |> then_(result =>
               (
                 switch (result) {
                 | WalletTypes.Ok(txId) =>
                   postMessage(
                     VentureWorkerMessage.SyncWallet(
                       ventureId,
                       [Event.Payout.Broadcast.make(~processId, ~txId)],
                       [],
                       [],
                       [],
                       [],
                       [],
                     ),
                   )
                 | WalletTypes.AlreadyInBlockchain =>
                   postMessage(
                     VentureWorkerMessage.SyncWallet(
                       ventureId,
                       [Event.Payout.Broadcast.make(~processId, ~txId)],
                       [],
                       [],
                       [],
                       [],
                       [],
                     ),
                   )
                 | WalletTypes.Error(errorMessage) =>
                   Utils.printError(
                     "Broadcasting transaction failed",
                     errorMessage,
                   );
                   postMessage(
                     VentureWorkerMessage.SyncWallet(
                       ventureId,
                       [],
                       [
                         Event.Payout.BroadcastFailed.make(
                           ~processId,
                           ~errorMessage,
                         ),
                       ],
                       [],
                       [],
                       [],
                       [],
                     ),
                   );
                 | WalletTypes.FetchError(_error) => ()
                 }
               )
               |> resolve
             )
          |> catchAndLogError
        )
      )
  );
type collector = {
  addresses: AddressCollector.t,
  transactions: TransactionCollector.t,
  walletInfo: WalletInfoCollector.t,
};
let make = () => {
  addresses: AddressCollector.make(),
  transactions: TransactionCollector.make(),
  walletInfo: WalletInfoCollector.make(),
};

let scanTransactions = ({addresses, transactions} as collector) =>
  Js.Promise.(
    all2((
      addresses.exposedAddresses
      |> NetworkClient.transactionInputs(addresses.network),
      NetworkClient.currentBlockHeight(addresses.network, ()),
    ))
    |> then_(((utxos, blockHeight)) =>
         (
           utxos
           ->Set.toArray
           ->(Array.mapU((. {txId}: Network.txInput) => txId))
           |> Set.String.mergeMany(transactions.transactionsOfInterest)
         )
         ->(
             Set.String.diff(
               transactions.confirmedTransactions
               |> Map.String.keysToArray
               |> Set.String.mergeMany(Set.String.empty),
             )
           )
         |> NetworkClient.transactionInfo(addresses.network)
         |> then_(txInfos =>
              (utxos, txInfos, blockHeight, collector) |> resolve
            )
       )
  );

let collectData = log =>
  log
  |> EventLog.reduce(
       ({addresses, transactions, walletInfo}, {event}: EventLog.item) => {
         addresses: addresses |> AddressCollector.apply(event),
         transactions: transactions |> TransactionCollector.apply(event),
         walletInfo: walletInfo |> WalletInfoCollector.apply(event),
       },
       make(),
     );

let filterUTXOs = (knownTxs, utxos) =>
  utxos
  ->(
      Set.keepU((. {txId}: Network.txInput) =>
        !knownTxs->(Set.String.has(txId))
      )
    );

let detectIncomeFromVenture = (ventureId, eventLog) => {
  logMessage(
    "Sychronizing wallet state for venture '"
    ++ VentureId.toString(ventureId)
    ++ "'",
  );
  Js.Promise.(
    eventLog
    |> collectData
    |> scanTransactions
    |> then_(((utxos, txInfos, blockHeight, {transactions, walletInfo})) => {
         walletInfo
         |> notifyOfUnlockedInputs(ventureId, blockHeight, transactions);
         transactions |> broadcastPayouts;
         let utxos = utxos |> filterUTXOs(transactions.knownIncomeTxs);
         let events =
           (utxos |> Set.toList)
           ->(
               List.mapU((. utxo: Network.txInput) =>
                 Event.Income.Detected.make(
                   ~address=utxo.address,
                   ~coordinates=utxo.coordinates,
                   ~txId=utxo.txId,
                   ~amount=utxo.value,
                   ~txOutputN=utxo.txOutputN,
                 )
               )
             );
         (
           switch (
             events,
             txInfos
             ->(
                 List.keepMapU(
                   (. {txId, blockHeight, unixTime}: WalletTypes.txInfo) =>
                   switch (blockHeight, unixTime) {
                   | (Some(blockHeight), Some(unixTime)) =>
                     Some(
                       Event.Transaction.Confirmed.make(
                         ~txId,
                         ~blockHeight,
                         ~unixTime,
                       ),
                     )
                   | _ => None
                   }
                 )
               ),
           ) {
           | ([], []) => ()
           | (_, confs) =>
             postMessage(
               VentureWorkerMessage.SyncWallet(
                 ventureId,
                 [],
                 [],
                 events,
                 [],
                 confs,
                 [],
               ),
             )
           }
         )
         |> resolve;
       })
  );
};

let syncWallets = (ventures: VentureId.map(EventLog.t)) =>
  ventures
  ->(
      Map.forEachU((. id, log) =>
        detectIncomeFromVenture(id, log) |> catchAndLogError
      )
    );
