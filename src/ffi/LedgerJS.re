type transport;
[@bs.module "@ledgerhq/hw-transport-u2f"] [@bs.scope "default"]
external createTransport : unit => Js.Promise.t(transport) = "create";

type error =
  | U2FNotSupported(string)
  | U2F_5(string)
  | Unknown;
let error =
  fun
  | U2FNotSupported(message) => "U2FNotSupported(" ++ message ++ ")"
  | U2F_5(message) => "U2F_5(" ++ message ++ ")"
  | Unknown => "Unknown";
let decodeError = error => {
  let error = error |> Obj.magic;
  switch (error##id) {
  | "U2FNotSupported" => U2FNotSupported(error##message)
  | "U2F_5" => U2F_5(error##message)
  | _ => Unknown
  };
};

type btc;

[@bs.module "@ledgerhq/hw-app-btc"] [@bs.new]
external btc : transport => btc = "default";

type ledgerPubKey = {
  .
  "publicKey": string,
  "bitcoinAddress": string,
  "chainCode": string,
};

[@bs.send]
external getWalletPublicKey : (btc, string) => Js.Promise.t(ledgerPubKey) =
  "";

type txInfo;

[@bs.send]
external splitTransaction :
  (btc, ~txHex: string, [@bs.as {json|true|json}] _) => txInfo =
  "";

[@bs.send]
external serializeTransactionOutputs : (btc, txInfo) => Node.buffer = "";

type inputInfo = (txInfo, int, string, int);

[@bs.send]
external signP2SHTransaction :
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
  "";
