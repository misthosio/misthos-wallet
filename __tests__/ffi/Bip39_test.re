open Jest;

open Expect;

open Bitcoin;

let () =
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
