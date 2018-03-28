Helpers.enableHttpRequests();

open Jest;

open Expect;

open Bitcoin;

let () = {
  let (keyA, keyB, keyC) = (
    ECPair.fromWIFWithNetwork(
      "cUVTgxrs44T7zVon5dSDicBkBRjyfLwL7RF1RvR7n94ar3HEaLs1",
      Networks.testnet
    ),
    ECPair.fromWIFWithNetwork(
      "cPfdeLvhwvAVRRM5wiEWopWviGG65gbxQCHdtFL56PYUJXsTYixf",
      Networks.testnet
    ),
    ECPair.fromWIFWithNetwork(
      "cPMRPo3fXGehCmFC5QsSFcZmYivsFtLVexxWi22CFwocvndXLqP1",
      Networks.testnet
    )
  );
  let chainCode =
    "c8bce5e6dac6f931af17863878cce2ca3b704c61b3d775fe56881cc8ff3ab1cb"
    |> Utils.bufFromHex;
  let masterA = HDNode.make(keyA, chainCode);
  let masterB = HDNode.make(keyB, chainCode);
  let masterC = HDNode.make(keyC, chainCode);
  let keyChain = Wallet.KeyChain.make(2, [masterA, masterB, masterC]);
  let address = Wallet.KeyChain.getAddress(0, keyChain);
  describe("KeyChain", () => {
    test("Creates a multi sig seg-wit address", () =>
      expect(address)
      |> toEqual(
           Wallet.Address.{
             path: [0, 0, 0],
             witnessScript: "522102e11b1b2f99100c211edec7cae96ae85c7a2777b92cd0111d9d38056f805f15f621031df3b277207004accd6e84a733f7e63a5af2a8adc932825de87c0c39d9ad36fb2103fd8ecf6599f5f252c47924cc98bb4ca6a474be487a1c96bd447ba34d8cb455fb53ae",
             redeemScript: "002004349691eaa6e094a73aa1c18d0a11453618920251799ee9acc069f07c366f98",
             address: "2NCWSAoWJwUdFnd9E8ENaT1zW9SUKoJviNj"
           }
         )
    );
    test("sorts the nodes", () => {
      let addressA =
        Wallet.KeyChain.make(2, [masterA, masterB, masterC])
        |> Wallet.KeyChain.getAddress(0);
      let addressB =
        Wallet.KeyChain.make(2, [masterA, masterB, masterC])
        |> Wallet.KeyChain.getAddress(0);
      expect(addressA) |> toEqual(addressB);
    });
  });
  describe("execute transaction", () => {
    let address =
      Wallet.makeAddress(~network=Networks.testnet, 2, [keyA, keyB, keyC]);
    Bitcoin.(
      testPromise(
        ~timeout=10000, "Can prepare, sign and finalize a transaction", () =>
        Js.Promise.(
          Helpers.faucet([
            (address.address, BTC.fromSatoshis(11000L)),
            (address.address, BTC.fromSatoshis(10100L))
          ])
          |> then_(utxos =>
               Wallet.preparePayoutTx(
                 address,
                 [
                   (
                     "mgWUuj1J1N882jmqFxtDepEC73Rr22E9GU",
                     BTC.fromSatoshis(9400L)
                   ),
                   (
                     "2N8qFbjFX4ZA1jTatE17kYZnS849NB9bN2T",
                     BTC.fromSatoshis(10100L)
                   )
                 ],
                 "reference",
                 keyA,
                 ~network=Networks.testnet
               )
             )
          |> then_((payoutTx: Wallet.payoutTx) => {
               let signature =
                 Wallet.signPayoutTx(
                   payoutTx,
                   keyC,
                   address,
                   ~network=Networks.testnet
                 );
               let tx =
                 Wallet.finalizeTx(
                   payoutTx,
                   [signature],
                   ~network=Networks.testnet
                 );
               /* Helpers.displayTx(Transaction.toHex(tx)); */
               Helpers.broadcastTransaction(
                 Wallet.finalizeTx(
                   payoutTx,
                   [signature],
                   ~network=Networks.testnet
                 )
               );
             })
          |> then_(result =>
               Helpers.getUTXOs(["2N8qFbjFX4ZA1jTatE17kYZnS849NB9bN2T"])
             )
          |> then_((utxos: list(BitcoindClient.utxo)) =>
               resolve(
                 expect(List.hd(utxos).amount)
                 |> toEqual(BTC.fromSatoshis(10100L))
               )
             )
        )
      )
    );
  });
};
