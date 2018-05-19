[@bs.val] external _postMessage : WebWorker.payload => unit = "postMessage";

let postMessage = msg =>
  {
    "msg": msg |> VentureWorkerMessage.encodeIncoming,
    "syncId": WebWorker.emptySyncId,
  }
  |> _postMessage;

open Belt;

open PrimitiveTypes;

let logLabel = "[Income Collection]";

let logMessage = WorkerUtils.logMessage(logLabel);

let catchAndLogError = WorkerUtils.catchAndLogError(logLabel);

let scanTransactions =
    ((addresses: AddressCollector.t, transactions: TransactionCollector.t)) =>
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
         |> then_(txInfos => (utxos, txInfos, transactions) |> resolve)
       )
  );

let findAddressesAndTxIds = log =>
  log
  |> EventLog.reduce(
       ((addresses, transactions), {event}: EventLog.item) => (
         addresses |> AddressCollector.apply(event),
         transactions |> TransactionCollector.apply(event),
       ),
       (AddressCollector.make(), TransactionCollector.make()),
     );

let filterUTXOs = (knownTxs, utxos) =>
  utxos
  |. List.keepMapU((. {txId} as utxo: Network.txInput) =>
       knownTxs |. Set.String.has(txId) ? None : Some(utxo)
     );

let detectIncomeFromVenture = (ventureId, eventLog) => {
  logMessage(
    "Detecting income for venture '" ++ VentureId.toString(ventureId) ++ "'",
  );
  Js.Promise.(
    eventLog
    |> findAddressesAndTxIds
    |> scanTransactions
    |> then_(((utxos, txInfos, transactions: TransactionCollector.t)) => {
         let utxos = utxos |> filterUTXOs(transactions.knownIncomeTxs);
         let events =
           utxos
           |. List.mapU((. utxo: Network.txInput) =>
                Event.IncomeDetected.make(
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
               VentureWorkerMessage.SyncWallet(ventureId, events, confs),
             )
           }
         )
         |> resolve;
       })
  );
};

let collectIncome = (ventures: VentureId.map(EventLog.t)) =>
  ventures
  |. Map.forEachU((. id, log) =>
       detectIncomeFromVenture(id, log) |> catchAndLogError
     );
