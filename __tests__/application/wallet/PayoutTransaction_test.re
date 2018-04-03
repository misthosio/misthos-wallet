Helpers.enableHttpRequests();

open Jest;

open Expect;

open Bitcoin;

open PrimitiveTypes;

open WalletTypes;

let () =
  describe("build", () => {
    let (keyA, keyB) = (
      ECPair.fromWIFWithNetwork(
        "cUVTgxrs44T7zVon5dSDicBkBRjyfLwL7RF1RvR7n94ar3HEaLs1",
        Networks.testnet
      ),
      ECPair.fromWIFWithNetwork(
        "cPfdeLvhwvAVRRM5wiEWopWviGG65gbxQCHdtFL56PYUJXsTYixf",
        Networks.testnet
      )
    );
    let chainCode =
      "c8bce5e6dac6f931af17863878cce2ca3b704c61b3d775fe56881cc8ff3ab1cb"
      |> Utils.bufFromHex;
    let masterA = HDNode.make(keyA, chainCode);
    let masterB = HDNode.make(keyB, chainCode);
    let ventureId = VentureId.fromString("test");
    let accountIdx = AccountIndex.first;
    let custodianKeyChainIdx = CustodianKeyChainIndex.first;
    let custodianKeyChainA =
      CustodianKeyChain.make(
        ~ventureId,
        ~accountIdx,
        ~keyChainIdx=custodianKeyChainIdx,
        ~masterKeyChain=masterA
      );
    let custodianPubKeyChainA =
      custodianKeyChainA |> CustodianKeyChain.toPublicKeyChain;
    let userA = UserId.fromString("userA.id");
    let accountKeyChainIdx = AccountKeyChainIndex.first;
    let accountKeyChain1 =
      AccountKeyChain.make(
        accountIdx,
        accountKeyChainIdx,
        1,
        [(userA, custodianPubKeyChainA)]
      );
    let firstAddress =
      AccountKeyChain.Address.Coordinates.firstExternal(accountKeyChain1);
    testPromise("hello", () =>
      expect(true) |> toEqual(true) |> Js.Promise.resolve
    );
  });
