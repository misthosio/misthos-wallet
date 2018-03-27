open Jest;

open Expect;

open PrimitiveTypes;

open Bitcoin;

let () =
  describe("getAddress", () => {
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
    let (masterA, masterB, masterC) = (
      HDNode.make(keyA, chainCode),
      HDNode.make(keyB, chainCode),
      HDNode.make(keyC, chainCode)
    );
    let ventureId = VentureId.fromString("test");
    let accountIndex = 0;
    let (cKeyChainA, cKeyChainB, cKeyChainC) = (
      CustodianKeyChain.make(
        ~ventureId,
        ~accountIndex,
        ~keyChainIndex=0,
        ~masterKeyChain=masterA
      )
      |> CustodianKeyChain.toPublicKeyChain,
      CustodianKeyChain.make(
        ~ventureId,
        ~accountIndex,
        ~keyChainIndex=0,
        ~masterKeyChain=masterB
      )
      |> CustodianKeyChain.toPublicKeyChain,
      CustodianKeyChain.make(
        ~ventureId,
        ~accountIndex,
        ~keyChainIndex=0,
        ~masterKeyChain=masterC
      )
      |> CustodianKeyChain.toPublicKeyChain
    );
    test("single Custodian", () => {
      let accountKeyChain =
        AccountKeyChain.make(
          1,
          [(UserId.fromString("custodianA"), cKeyChainA)]
        );
      expect(accountKeyChain |> AccountKeyChain.getAddress(0))
      |> toEqual(
           AccountKeyChain.Address.{
             path: [0, 0, 0],
             witnessScript: "51210358ebee38e86598266dc351dfec81c0bd98e3a90a4e93bff72003569f2b02d13351ae",
             redeemScript: "002027fa0596838478a59b5c0512acf480fdba510cd320def9e3d9e9d27a13b7e72f",
             address: "2N3gWQwj2RrHaw7rWmbr1vKkzBnutSMp2LE"
           }
         );
    });
  });
