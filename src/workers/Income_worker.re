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

let scanTransactions = ((addresses: AddressCollector.t, txIds)) =>
  Js.Promise.(
    addresses.exposedAddresses
    |> Network.transactionInputs(addresses.network)
    |> then_(utxos => (txIds, utxos) |> resolve)
  );

let findAddressesAndTxIds =
  EventLog.reduce(
    ((addresses, txIds), {event}: EventLog.item) => {
      let addresses = addresses |> AddressCollector.apply(event);
      switch (event) {
      | Event.IncomeDetected({txId, txOutputN}) => (
          addresses,
          txIds |. Belt.Set.String.add(txId ++ string_of_int(txOutputN)),
        )
      | _ => (addresses, txIds)
      };
    },
    (AddressCollector.make(), Belt.Set.String.empty),
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
    |> then_(((knownTxs, utxos)) => {
         let events =
           utxos
           |. Belt.List.keepMapU((. utxo: Network.txInput) => {
                let txOutId = utxo.txId ++ string_of_int(utxo.txOutputN);
                knownTxs |. Belt.Set.String.has(txOutId) ?
                  None :
                  Some(
                    Event.IncomeDetected.make(
                      ~address=utxo.address,
                      ~coordinates=utxo.coordinates,
                      ~txId=utxo.txId,
                      ~amount=utxo.value,
                      ~txOutputN=utxo.txOutputN,
                    ),
                  );
              });
         (
           switch (events) {
           | [] => ()
           | _ =>
             postMessage(
               VentureWorkerMessage.IncomeDetected(ventureId, events),
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

let fiveSecondsInMilliseconds = 5000;

let syncInterval = fiveSecondsInMilliseconds;

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
