[@bs.val]
external bufFromStringWithEncoding :
  (Js.String.t, ~encoding: Js.String.t) => Node.buffer =
  "Buffer.from";

type transport;

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
             bufFromStringWithEncoding(pubKey##publicKey, ~encoding="hex"),
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
