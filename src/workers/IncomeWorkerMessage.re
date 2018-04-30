type exposedAddresses = list(string);

type txIds = list(string);

type send =
  | Wait
  | MonitorAddresses(exposedAddresses, txIds);

let msgType =
  fun
  | Wait => "Wait"
  | MonitorAddresses(_, _) => "MonitorAddresses";

type output = {
  address: string,
  amount: Js.Json.t,
};

type transaction = {
  txId: string,
  outputs: list(output),
};

type receive =
  | NewTransactionsDetected(list(transaction));

let decodeTransactions = (transactions: list(transaction)) =>
  transactions
  |> List.map(tx =>
       (
         {
           txId: tx.txId,
           outputs:
             tx.outputs
             |> List.map((o: output) =>
                  (
                    {address: o.address, amount: o.amount |> BTC.decode}: WalletTypes.output
                  )
                ),
         }: WalletTypes.transaction
       )
     );
