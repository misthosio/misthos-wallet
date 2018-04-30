open WalletTypes;

module Message = IncomeWorkerMessage;

type self;

[@bs.val] external self : self = "";

[@bs.set]
external onMessage :
  (self, [@bs.uncurry] ({. "data": Message.send} => unit)) => unit =
  "onmessage";

[@bs.val] external postMessage : Message.receive => unit = "postMessage";

let logMessage = msg => Js.log("[Income Worker] - " ++ msg);

let tenSecondsInMilliseconds = 10000;

let syncInterval = tenSecondsInMilliseconds;

let interval = ref(None);

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

let scanTransactions = (addresses, txIds) =>
  addresses
  |> List.iter(address =>
       Js.Promise.(
         fetchTransactionsForAddress(address)
         |> then_(txs => {
              let newTransactions =
                txs
                |> List.filter(tx =>
                     tx.outputs |> List.exists(o => o.address == address)
                   )
                |> List.filter(tx => txIds |> List.mem(tx.txId) == false);
              (
                if (newTransactions |> List.length > 0) {
                  postMessage(
                    NewTransactionsDetected(
                      newTransactions
                      |> List.map(tx =>
                           (
                             {
                               txId: tx.txId,
                               outputs:
                                 tx.outputs
                                 |> List.map(o =>
                                      (
                                        {
                                          address: o.address,
                                          amount: o.amount |> BTC.encode,
                                        }: Message.output
                                      )
                                    ),
                             }: Message.transaction
                           )
                         ),
                    ),
                  );
                }
              )
              |> resolve;
            })
       )
       |> ignore
     );

let handleMsg = msg => {
  logMessage("Received message '" ++ Message.msgType(msg) ++ "'");
  switch (interval^) {
  | Some(id) => Js.Global.clearInterval(id)
  | None => ()
  };
  switch (msg) {
  | MonitorAddresses(exposedAddresses, txIds) =>
    logMessage("Scanning transactions");
    scanTransactions(exposedAddresses, txIds);
    interval :=
      Some(
        Js.Global.setInterval(
          () => scanTransactions(exposedAddresses, txIds),
          syncInterval,
        ),
      );
  | Wait =>
    switch (interval^) {
    | Some(id) => Js.Global.clearInterval(id)
    | None => ()
    }
  };
};

onMessage(self, msg => handleMsg(msg##data));
