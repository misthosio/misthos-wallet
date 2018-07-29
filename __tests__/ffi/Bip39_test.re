open Jest;

open Expect;

open Bitcoin;

let () = {
  test("Mnemonic from pubKey", () => {
    let keyPair =
      ECPair.fromWIFWithNetwork(
        "cPfdeLvhwvAVRRM5wiEWopWviGG65gbxQCHdtFL56PYUJXsTYixf",
        Networks.testnet,
      );
    let pubKey =
      keyPair
      |> Utils.publicKeyFromKeyPair
      |. String.sub(0, 64)
      |> Utils.bufFromHex;
    let wordList = Bip39.entropyToMnemonic(pubKey, Bip39.Wordlist.english);
    expect(wordList)
    |> toEqual(
         "acoustic fuel evolve talk wine steak pool wrist heavy april refuse include material crane bargain rigid type carbon image spike sword tissue wrong pottery",
       );
  });
  test("stuff", () => {
    open Bitcoin;
    let wordList = "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about";
    let node =
      Bip39.mnemonicToSeed(wordList)
      |. HDNode.fromSeedBuffer(Networks.bitcoin)
      |> HDNode.derivePath("0'/0")
      |> HDNode.derive(0);
    let lPubKeyBuf =
      "046666422d00f1b308fc7527198749f06fedb028b979c09f60d0348ef79c985e4138b86996b354774c434488d61c7fb20a83293ef3195d422fde9354e6cf2a74ce"
      |> Utils.bufFromHex
      |> ECurve.Point.decodeFrom(ECurve.secp256k1)
      |> ECurve.Point.getEncoded(true);
    Js.log2("node asht", node |> HDNode.getPublicKeyBuffer |> Utils.bufToHex);
    Js.log2("ledger", lPubKeyBuf |> Utils.bufToHex);
    let lPubKey = lPubKeyBuf |> ECPair.fromPublicKeyBuffer;
    let chainCode =
      "bce80dd580792cd18af542790e56aa813178dc28644bb5f03dbd44c85f2d2e7a"
      |> Utils.bufFromHex;
    let lNode = HDNode.make(lPubKey, chainCode);
    let derivedAddress = lNode##keyPair |> ECPair.getAddress;
    Js.log(derivedAddress);
    let pubKey =
      node |> HDNode.getPublicKeyBuffer |> ECPair.fromPublicKeyBuffer;
    expect(pubKey |> ECPair.getAddress)
    |> toEqual(lPubKey |> ECPair.getAddress);
  });
};
