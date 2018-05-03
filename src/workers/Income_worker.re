[%bs.raw {| self.localStorage = require("./fakeLocalStorage").localStorage |}];

[%bs.raw
  {| self.window = { localStorage: self.localStorage , location: { origin: self.origin } } |}
];

open PrimitiveTypes;

open WalletTypes;

module Message = IncomeWorkerMessage;

type self;

[@bs.val] external self : self = "";

[@bs.set]
external onMessage :
  (self, [@bs.uncurry] ({. "data": Message.incoming} => unit)) => unit =
  "onmessage";

[@bs.val] external postMessage : Message.outgoing => unit = "postMessage";

let logMessage = msg => Js.log("[Income Worker] - " ++ msg);

let testnetApiEndpoint = "https://testnet-api.smartbit.com.au/v1/blockchain/address";

let decodeResponse = raw =>
  Json.Decode.(
    raw
    |> withDefault(
         [],
         field("address", address =>
           address
           |> field("transactions", array(SmartbitClient.decodeTransaction))
           |> Array.to_list
         ),
       )
  );

let fetchTransactionsForAddress = address =>
  Js.Promise.(
    Fetch.(
      fetch(testnetApiEndpoint ++ "/" ++ address)
      |> then_(res =>
           if (res |> Response.status == 200) {
             res
             |> Response.json
             |> then_(json => json |> decodeResponse |> resolve);
           } else {
             [] |> resolve;
           }
         )
    )
  );

let scanTransactions = ((addresses, txIds)) =>
  Js.Promise.(
    addresses
    |> List.map(address =>
         fetchTransactionsForAddress(address)
         |> then_(txs =>
              txs
              |> List.filter(tx =>
                   tx.outputs |> List.exists(o => o.address == address)
                 )
              |> List.filter(tx => txIds |> List.mem(tx.txId) == false)
              |> resolve
            )
       )
    |> Array.of_list
    |> all
    |> then_(txs =>
         (addresses, txs |> Array.to_list |> List.flatten) |> resolve
       )
  );

let findAddressesAndTxIds =
  EventLog.reduce(
    ((addresses, txIds), {event}: EventLog.item) =>
      switch (event) {
      | Event.IncomeDetected({txId}) => (addresses, [txId, ...txIds])
      | Event.IncomeAddressExposed({address}) => (
          [address, ...addresses],
          txIds,
        )
      | _ => (addresses, txIds)
      },
    ([], []),
  );

let detectIncomeFromTransaction = addresses =>
  List.map(tx =>
    tx.outputs
    |> List.filter(o => addresses |> List.mem(o.address))
    |> List.map(out =>
         Event.IncomeDetected.make(
           ~address=out.address,
           ~txId=tx.txId,
           ~amount=out.amount,
         )
         |> Event.IncomeDetected.encode
       )
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
    |> then_(((addresses, transactions)) =>
         transactions
         |> detectIncomeFromTransaction(addresses)
         |> List.map(events =>
              postMessage(TransactionDetected(ventureId, events))
            )
         |> ignore
         |> resolve
       )
  );
};

let detectIncomeFromAll = () => {
  logMessage("Detecting income");
  Js.Promise.(
    Session.getCurrentSession()
    |> then_(
         fun
         | Session.LoggedIn(_data) =>
           Venture.Index.load()
           |> then_(index =>
                index
                |> List.iter(({id}: Venture.Index.item) =>
                     detectIncomeFromVenture(id) |> ignore
                   )
                |> resolve
              )
         | _ => resolve(),
       )
  );
};

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
