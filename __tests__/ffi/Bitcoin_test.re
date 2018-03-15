open Jest;

open Expect;

open Bitcoin;

let () = {
  describe("Crypto", () =>
    test("sha256", () =>
      expect(Crypto.sha256("hello") |> BufferExt.toStringWithEncoding("hex"))
      |> toBe(
           "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"
         )
    )
  );
  describe("ECPair", () => {
    test("fromWIF", () => {
      let wif = "KwDiBf89QgGbjEhKnhXJuH7LrciVrZi3qYjgd9M7rFU73sVHnoWn";
      let keyPair = wif |> ECPair.fromWIF;
      expect(keyPair |> ECPair.getNetwork) |> toBe(Networks.bitcoin);
    });
    test("makeRandomWithNetwork", () => {
      let keyPair = ECPair.makeRandomWithNetwork(Networks.testnet);
      expect(keyPair |> ECPair.getNetwork) |> toBe(Networks.testnet);
    });
    test("sign/verify", () => {
      let keyPair = ECPair.makeRandom();
      let der =
        keyPair
        |> ECPair.sign(Crypto.sha256("hello"))
        |> ECSignature.toDER
        |> BufferExt.toStringWithEncoding("hex");
      let signature =
        ECSignature.fromDER(
          BufferExt.fromStringWithEncoding(der, ~encoding="hex")
        );
      let verified =
        keyPair |> ECPair.verify(Crypto.sha256("hello"), signature);
      expect(verified) |> toEqual(Js.true_);
    });
  });
  describe("TransactionBuilder", () =>
    test("example", () => {
      /* taken from https://bitcoinjs.org/ */
      let wif = "L1uyy5qTuGrVXrmrsvHWHgVzW9kKdrp27wBC7Vs6nZDTF2BRUVwy";
      let keyPair = ECPair.fromWIF(wif);
      let tx = TxBuilder.create();
      let txId = "aa94ab02c182214f090e99a0d57021caffd0f195a81c24602b1028b130b63e31";
      tx |> TxBuilder.addInput(txId, 0) |> ignore;
      tx
      |> TxBuilder.addOutput("1Gokm82v6DmtwKEB8AiVhm82hyFSsEvBDK", 15000.)
      |> ignore;
      tx |> TxBuilder.sign(0, keyPair);
      let hex = tx |> TxBuilder.build |> Transaction.toHex;
      expect(hex) |> toMatch("0100000001313eb630b128102b60241ca895f1d0ffca21");
    })
  );
};
