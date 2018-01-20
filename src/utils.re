let bufToHex = BufferExt.toStringWithEncoding("hex");

let bufFromHex = BufferExt.fromStringWithEncoding(~encoding="hex");

let keyPairFromPrivateKey = key =>
  Bitcoin.(key |> BigInteger.fromHex |> ECPair.create);

let publicKeyFromKeyPair = pair =>
  Bitcoin.(pair |> ECPair.getPublicKeyBuffer |> bufToHex);

let keyFromPublicKey = key =>
  key |> bufFromHex |> Bitcoin.ECPair.fromPublicKeyBuffer;

let addressFromPublicKey = pubKey =>
  pubKey
  |> bufFromHex
  |> Bitcoin.ECPair.fromPublicKeyBuffer
  |> Bitcoin.ECPair.getAddress;

let signatureToString = ecSignature =>
  ecSignature |> Bitcoin.ECSignature.toDER |> bufToHex;

let signatureFromString = ecSignature =>
  ecSignature |> bufFromHex |> Bitcoin.ECSignature.fromDER;

let hash = s => s |> Bitcoin.Crypto.sha256 |> bufToHex;

[@bs.val] [@bs.scope "window.location"] external origin : string = "origin";

let (>>) = (f, g, v) => v |> f |> g;

let printError = (message, error) => {
  Js.log("Error - " ++ message ++ ":");
  Js.log(error);
};
