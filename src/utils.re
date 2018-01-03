let toHex = (buffer) => buffer |> BufferExt.toStringWithEncoding("hex");

let keyPairFromPrivateKey = (key) => Bitcoin.(key |> BigInteger.fromHex |> ECPair.create);

let publicKeyFromKeyPair = (pair) => Bitcoin.(pair |> ECPair.getPublicKeyBuffer |> toHex);
