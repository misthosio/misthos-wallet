[@bs.val]
external bufFromStringWithEncoding :
  (Js.String.t, ~encoding: Js.String.t) => Node.buffer =
  "Buffer.from";

type transport;
[@bs.module "@ledgerhq/hw-transport-u2f"] [@bs.scope "default"]
external createBrowserTransport : unit => Js.Promise.t(transport) = "create";

type transportError =
  | U2FNotSupported(string)
  | U2F_5(string)
  | Unknown;
let transportErrorToString =
  fun
  | U2FNotSupported(message) => "U2FNotSupported(" ++ message ++ ")"
  | U2F_5(message) => "U2F_5(" ++ message ++ ")"
  | Unknown => "Unknown";
let decodeTransportError = error => {
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

let getHDNode = (path, network, ledger) =>
  Js.Promise.(
    ledger
    |. getWalletPublicKey(path)
    |> then_(pubKey =>
         Bitcoin.HDNode.fromPublicKey(
           ~publicKey=
             bufFromStringWithEncoding(pubKey##publicKey, ~encoding="hex")
             |. Bitcoin.ECPair.fromPublicKey({"network": network})
             |> Bitcoin.ECPair.getPublicKey,
           ~chainCode=
             bufFromStringWithEncoding(pubKey##chainCode, ~encoding="hex"),
           network,
         )
         |> resolve
       )
  );

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
