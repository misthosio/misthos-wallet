type transport;
[@bs.module "@ledgerhq/hw-transport-u2f"] [@bs.scope "default"]
external createTransport: unit => Js.Promise.t(transport) = "create";

type error =
  | Message(string)
  | Unknown;
let errorToString =
  fun
  | Message(message) => message
  | Unknown => "Unknown";
let decodeError = error => {
  let error = error |> Obj.magic;
  try (Message(error##message)) {
  | _ => Unknown
  };
};

type btc;

[@bs.module "@ledgerhq/hw-app-btc"] [@bs.new]
external btc: transport => btc = "default";

type ledgerPubKey = {
  .
  "publicKey": string,
  "bitcoinAddress": string,
  "chainCode": string,
};

[@bs.send]
external getWalletPublicKey: (btc, string) => Js.Promise.t(ledgerPubKey) = "getWalletPublicKey";

type txInfo;
[@bs.send]
external splitTransaction: (btc, string, [@bs.as {json|true|json}] _) => txInfo =
  "splitTransaction";

[@bs.send]
external serializeTransactionOutputs: (btc, txInfo) => Node.buffer = "serializeTransactionOutputs";

type inputInfo = (txInfo, int, string, int);

[@bs.send]
external signP2SHTransaction:
  (
    btc,
    array(inputInfo),
    array(string),
    string,
    [@bs.as 0] _,
    [@bs.as 1] _,
    [@bs.as {json|true|json}] _,
    [@bs.as 2] _
  ) =>
  Js.Promise.t(array(string)) =
  "signP2SHTransaction";
