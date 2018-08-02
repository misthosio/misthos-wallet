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
