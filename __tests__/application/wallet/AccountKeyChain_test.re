open Jest;

open Expect;

open PrimitiveTypes;

open WalletTypes;

open Bitcoin;

let () =
  describe("getAddress", () => {
    let (keyA, keyB, keyC) = (
      ECPair.fromWIFWithNetwork(
        "cUVTgxrs44T7zVon5dSDicBkBRjyfLwL7RF1RvR7n94ar3HEaLs1",
        Networks.testnet,
      ),
      ECPair.fromWIFWithNetwork(
        "cPfdeLvhwvAVRRM5wiEWopWviGG65gbxQCHdtFL56PYUJXsTYixf",
        Networks.testnet,
      ),
      ECPair.fromWIFWithNetwork(
        "cPMRPo3fXGehCmFC5QsSFcZmYivsFtLVexxWi22CFwocvndXLqP1",
        Networks.testnet,
      ),
    );
    let chainCode =
      "c8bce5e6dac6f931af17863878cce2ca3b704c61b3d775fe56881cc8ff3ab1cb"
      |> Utils.bufFromHex;
    let (masterA, masterB, masterC) = (
      HDNode.make(keyA, chainCode),
      HDNode.make(keyB, chainCode),
      HDNode.make(keyC, chainCode),
    );
    let ventureId = VentureId.fromString("test");
    let accountIdx = AccountIndex.default;
    let keyChainIdx = CustodianKeyChainIndex.first;
    let (cKeyChainA, _cKeyChainB, _cKeyChainC) = (
      CustodianKeyChain.make(
        ~ventureId,
        ~accountIdx,
        ~keyChainIdx,
        ~masterKeyChain=masterA,
      )
      |> CustodianKeyChain.toPublicKeyChain,
      CustodianKeyChain.make(
        ~ventureId,
        ~accountIdx,
        ~keyChainIdx,
        ~masterKeyChain=masterB,
      )
      |> CustodianKeyChain.toPublicKeyChain,
      CustodianKeyChain.make(
        ~ventureId,
        ~accountIdx,
        ~keyChainIdx,
        ~masterKeyChain=masterC,
      )
      |> CustodianKeyChain.toPublicKeyChain,
    );
    test("single Custodian", () => {
      let accountKeyChain =
        AccountKeyChain.make(
          AccountIndex.first,
          AccountKeyChainIndex.first,
          1,
          [(UserId.fromString("custodianA"), cKeyChainA)],
        );
      let firstCoordinates =
        accountKeyChain |> AccountKeyChain.Address.Coordinates.firstExternal;
      expect(
        accountKeyChain |> AccountKeyChain.Address.make(firstCoordinates),
      )
      |> toEqual(
           {
             nCoSigners: 1,
             nPubKeys: 1,
             coordinates: (
               AccountIndex.first,
               AccountKeyChainIndex.first,
               ChainIndex.externalChain,
               AddressIndex.first,
             ),
             witnessScript: "512103331e2cc5405b722e54b4c64ce11e149906a8af27f6126eb2ded2f0a780a1406c51ae",
             redeemScript: "002097eab88cae50436c7588ce328aa33b139edcf8dd833cdf4cf8e80fbc31b8a0f3",
             address: "2NCzQPvZTdvtyu3pkqyMKGNgoTNBvFHNL3n",
           }: AccountKeyChain.Address.t,
         );
    });
  });
