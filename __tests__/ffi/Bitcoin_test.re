open Belt;

open Jest;
open Expect;

open Bitcoin;

let () = {
  let network = Networks.testnet;
  let pair =
    ECPair.fromWIFWithNetwork(
      "92Qba5hnyWSn5Ffcka56yMQauaWY6ZLd91Vzxbi4a9CCetaHtYj",
      network,
    );
  describe("ECPair", () => {
    test("can access public key", () =>
      expect(pair |> Utils.publicKeyFromKeyPair)
      |> toEqual(
           "044289801366bcee6172b771cf5a7f13aaecd237a0b9a1ff9d769cabc2e6b70a34cec320a0565fb7caf11b1ca2f445f9b7b012dda5718b3cface369ee3a034ded6",
         )
    );
    test("can access network", () =>
      expect(pair |> ECPair.getNetwork) |> toEqual(network)
    );
  });
  describe("Address", () =>
    test("can return an address from a key pair", () =>
      expect(pair |> Address.fromKeyPair)
      |> toEqual("mgWUuj1J1N882jmqFxtDepEC73Rr22E9GU")
    )
  );
  describe("Payments", () => {
    let keys = [|
      ECPair.fromWIFWithNetwork(
        "cUVTgxrs44T7zVon5dSDicBkBRjyfLwL7RF1RvR7n94ar3HEaLs1",
        network,
      ),
      ECPair.fromWIFWithNetwork(
        "cPMRPo3fXGehCmFC5QsSFcZmYivsFtLVexxWi22CFwocvndXLqP1",
        network,
      ),
      ECPair.fromWIFWithNetwork(
        "cPfdeLvhwvAVRRM5wiEWopWviGG65gbxQCHdtFL56PYUJXsTYixf",
        network,
      ),
    |];
    test("multisig", () => {
      let ret =
        Payments.scriptHash({
          "redeem":
            Payments.witnessScriptHash({
              "redeem":
                Payments.multisig({
                  "m": 2,
                  "pubkeys": keys |. Array.map(ECPair.getPublicKey),
                  "network": network,
                }),
              "network": network,
            }),
          "network": network,
        });
      expect(ret##address) |> toEqual("2Mw8JgQijki6NkAwGZiWa4XdnE4be1H93ku");
    });
  });
  describe("HDNode", () => {
    let pubkey = pair |> Utils.publicKeyFromKeyPair;
    let chainCode = pubkey |. String.sub(0, 64) |> Utils.bufFromHex;
    test("can create an HDNode", () => {
      let node =
        HDNode.fromPrivateKey(
          ~privateKey=pair |> ECPair.getPrivateKey,
          ~chainCode,
          pair |> ECPair.getNetwork,
        );
      expect(node |> HDNode.getPublicKey |> Utils.bufToHex)
      |> toEqual(
           "024289801366bcee6172b771cf5a7f13aaecd237a0b9a1ff9d769cabc2e6b70a34",
         );
    });
  });
};
