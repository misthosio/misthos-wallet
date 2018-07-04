[@bs.val] external _postMessage : WebWorker.message => unit = "postMessage";

let postMessage = msg =>
  {"payload": msg |> VentureWorkerMessage.encodeIncoming, "correlationId": ""}
  |> _postMessage;

open Belt;

open PrimitiveTypes;

let logLabel = "[Wallet Sync]";

let logMessage = WorkerUtils.logMessage(logLabel);

let catchAndLogError = WorkerUtils.catchAndLogError(logLabel);

let broadcastPayouts =
    ({ventureId, network, notYetBroadcastPayouts}: TransactionCollector.t) =>
  Js.Promise.(
    notYetBroadcastPayouts
    |. Map.forEachU(
         (. processId, {txId, payoutTx: {txHex}}: Event.Payout.Finalized.t) =>
         txHex
         |> Bitcoin.Transaction.fromHex
         |> Network.broadcastTransaction(network)
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
                    ),
                  );
                | WalletTypes.FetchError(_error) => ()
                }
              )
              |> resolve
            )
         |> catchAndLogError
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
    addresses.exposedAddresses
    |> Network.transactionInputs(addresses.network)
    |> then_(utxos =>
         utxos
         |. List.mapU((. {txId}: Network.txInput) => txId)
         |> List.toArray
         |> Set.String.mergeMany(transactions.transactionsOfInterest)
         |> Set.String.diff(_, transactions.confirmedTransactions)
         |> Network.transactionInfo(addresses.network)
         |> then_(txInfos => (utxos, txInfos, collector) |> resolve)
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
  |. List.keepMapU((. {txId} as utxo: Network.txInput) =>
       knownTxs |. Set.String.has(txId) ? None : Some(utxo)
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
    |> then_(((utxos, txInfos, {transactions})) => {
         transactions |> broadcastPayouts;
         let utxos = utxos |> filterUTXOs(transactions.knownIncomeTxs);
         let events =
           utxos
           |. List.mapU((. utxo: Network.txInput) =>
                Event.Income.Detected.make(
                  ~address=utxo.address,
                  ~coordinates=utxo.coordinates,
                  ~txId=utxo.txId,
                  ~amount=utxo.value,
                  ~txOutputN=utxo.txOutputN,
                )
              );
         (
           switch (
             events,
             txInfos
             |. List.keepMapU(
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
  |. Map.forEachU((. id, log) =>
       detectIncomeFromVenture(id, log) |> catchAndLogError
     );
