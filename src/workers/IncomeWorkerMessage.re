open WalletTypes;

type exposedAddresses = list(string);

type txIds = list(string);

type incoming =
  | Wait
  | MonitorAddresses(exposedAddresses, txIds);

let msgType =
  fun
  | Wait => "Wait"
  | MonitorAddresses(_, _) => "MonitorAddresses";

type outgoing =
  | NewTransactionsDetected(list(transaction));

type encodedOutgoing = Js.Json.t;

let encodeOutgoing =
  fun
  | NewTransactionsDetected(transactions) =>
    Json.Encode.list(WalletTypes.encodeTransaction, transactions);

let decodeOutgoing = raw =>
  NewTransactionsDetected(
    raw |> Json.Decode.list(WalletTypes.decodeTransaction),
  );
