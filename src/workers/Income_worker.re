[%bs.raw {| self.localStorage = require("./fakeLocalStorage").localStorage |}];

[%bs.raw
  {| self.window = { localStorage: self.localStorage , location: { origin: self.origin } } |}
];

module Message = IncomeWorkerMessage;

type self;

[@bs.val] external self : self = "";

[@bs.set]
external onMessage :
  (self, [@bs.uncurry] ({. "data": Message.incoming} => unit)) => unit =
  "onmessage";

[@bs.val] external _postMessage : Js.Json.t => unit = "postMessage";

let postMessage = msg =>
  msg |> VentureWorkerMessage.encodeIncoming |> _postMessage;

open Belt;

open PrimitiveTypes;

let logMessage = msg => Js.log("[Income Worker] - " ++ msg);

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

let findAddressesAndTxIds =
  EventLog.reduce(
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

let detectIncomeFromVenture = ventureId => {
  logMessage(
    "Detecting income for venture '" ++ VentureId.toString(ventureId) ++ "'",
  );
  Js.Promise.(
    WorkerUtils.loadVenture(ventureId)
    |> then_(eventLog =>
         eventLog |> findAddressesAndTxIds |> scanTransactions
       )
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
           switch (events, txInfos) {
           | ([], []) => ()
           | _ =>
             postMessage(
               VentureWorkerMessage.SyncWallet(
                 ventureId,
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
               ),
             )
           }
         )
         |> resolve;
       })
  );
};

let detectIncomeFromAll = () =>
  Js.Promise.(
    Session.getCurrentSession()
    |> then_(
         fun
         | Session.LoggedIn(_data) =>
           Venture.Index.load()
           |> then_(index =>
                index
                |. List.forEach(({id}: Venture.Index.item) =>
                     detectIncomeFromVenture(id) |> ignore
                   )
                |> resolve
              )
         | _ => resolve(),
       )
    |> catch(err => {
         logMessage("Error while syncing:");
         Js.log(err);
         resolve();
       })
  );

let tenSecondsInMilliseconds = 10000;

let syncInterval = tenSecondsInMilliseconds;

let handleMsg =
  fun
  | Message.UpdateSession(items) => {
      logMessage("Handling 'UpdateSession'");
      items |> WorkerLocalStorage.setBlockstackItems;
      detectIncomeFromAll() |> ignore;
      Js.Global.setInterval(
        () => detectIncomeFromAll() |> ignore,
        syncInterval,
      );
    };

let intervalId: ref(option(Js.Global.intervalId)) = ref(None);

onMessage(
  self,
  msg => {
    let newIntervalid = handleMsg(msg##data);
    intervalId^
    |> Utils.mapOption(id =>
         if (newIntervalid != id) {
           Js.Global.clearInterval(id);
         }
       )
    |> ignore;
    intervalId := Some(newIntervalid);
  },
);
