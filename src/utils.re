let bufToHex = BufferExt.toStringWithEncoding("hex");

let bufFromHex = BufferExt.fromStringWithEncoding(~encoding="hex");

let keyPairFromPrivateKey = key =>
  Bitcoin.(key |> BigInteger.fromHex |> ECPair.create);

let publicKeyFromKeyPair = pair =>
  Bitcoin.(pair |> ECPair.getPublicKeyBuffer |> bufToHex);

let signatureToString = ecSignature =>
  ecSignature |> Bitcoin.ECSignature.toDER |> bufToHex;

let signatureFromString = ecSignature =>
  ecSignature |> bufFromHex |> Bitcoin.ECSignature.fromDER;

let hash = s => s |> Bitcoin.Crypto.sha256 |> bufToHex;
