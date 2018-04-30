open WalletTypes;

type exposedAddresses = list(string);

type txIds = list(string);

type send =
  | Wait
  | MonitorAddresses(exposedAddresses, txIds);

let msgType =
  fun
  | Wait => "Wait"
  | MonitorAddresses(_, _) => "MonitorAddresses";

type receive =
  | NewTransactionsDetected(list(transaction));

let encodeReceive =
  fun
  | NewTransactionsDetected(transactions) =>
    Json.Encode.list(WalletTypes.encodeTransaction, transactions);

let decodeReceive = raw =>
  NewTransactionsDetected(
    raw |> Json.Decode.list(WalletTypes.decodeTransaction),
  );
