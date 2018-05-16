let bufToHex = BufferExt.toStringWithEncoding("hex");

let bufFromHex = BufferExt.fromStringWithEncoding(~encoding="hex");

let hexByteLength = BufferExt.byteLength(~encoding="hex");

let keyPairFromPrivateKey = (network, key) =>
  Bitcoin.(ECPair.makeWithNetwork(key |> BigInteger.fromHex, network));

let publicKeyFromKeyPair = pair =>
  Bitcoin.(pair |> ECPair.getPublicKeyBuffer |> bufToHex);

let keyFromPublicKey = key =>
  key |> bufFromHex |> Bitcoin.ECPair.fromPublicKeyBuffer;

let signatureToString = ecSignature =>
  ecSignature |> Bitcoin.ECSignature.toDER |> bufToHex;

let signatureFromString = ecSignature =>
  ecSignature |> bufFromHex |> Bitcoin.ECSignature.fromDER;

let hash = s => s |> Bitcoin.Crypto.sha256 |> bufToHex;

let hashCode = s =>
  (
    s
    |> Js.String.castToArrayLike
    |> Js.Array.from
    |> Array.to_list
    |> List.fold_left(
         (h, c) => {
           let h = h lsl 5 - h + (c.[0] |> Char.code);
           h land h;
         },
         0,
       )
  )
  land 0x7fffffff;

let (>>) = (f, g, v) => v |> f |> g;

let printError = (message, error) => {
  Js.log("Error - " ++ message ++ ":");
  Js.log(error);
};

let mapOption = fn =>
  fun
  | Some(a) => Some(fn(a))
  | None => None;

let encodeFloat = Json.Encode.float;

let decodeFloat = Json.Decode.float;
