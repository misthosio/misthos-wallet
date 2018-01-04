let toHex = buffer => buffer |> BufferExt.toStringWithEncoding("hex");

let fromHex = hex => hex |> BufferExt.fromStringWithEncoding(~encoding="hex");

let keyPairFromPrivateKey = key =>
  Bitcoin.(key |> BigInteger.fromHex |> ECPair.create);

let publicKeyFromKeyPair = pair =>
  Bitcoin.(pair |> ECPair.getPublicKeyBuffer |> toHex);

let signatureToString = ecSignature =>
  ecSignature |> Bitcoin.ECSignature.toDER |> toHex;
