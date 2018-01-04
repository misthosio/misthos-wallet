let bufToHex = buffer => buffer |> BufferExt.toStringWithEncoding("hex");

let bufFromHex = hex =>
  hex |> BufferExt.fromStringWithEncoding(~encoding="hex");

let keyPairFromPrivateKey = key =>
  Bitcoin.(key |> BigInteger.fromHex |> ECPair.create);

let publicKeyFromKeyPair = pair =>
  Bitcoin.(pair |> ECPair.getPublicKeyBuffer |> bufToHex);

let signatureToString = ecSignature =>
  ecSignature |> Bitcoin.ECSignature.toDER |> bufToHex;
