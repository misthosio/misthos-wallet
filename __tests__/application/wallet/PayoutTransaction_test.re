Helpers.enableHttpRequests();

open Jest;

open Expect;

open Bitcoin;

open PrimitiveTypes;

open WalletTypes;

let () = {
  describe("build", () => {
    let inputs: list(Network.txInput) = [
      {
        txId: "d66c39a24f63d80c13e44cf1ce562618d1d0d92675118aa331e5367a7ddb9de7",
        txOutputN: 0,
        address: "2N3gWQwj2RrHaw7rWmbr1vKkzBnutSMp2LE",
        value: BTC.fromSatoshis(10000L),
        nCoSigners: 1,
        confirmations: 6,
        coordinates: (
          AccountIndex.first,
          AccountKeyChainIndex.first,
          ChainIndex.externalChain,
          AddressIndex.first
        )
      },
      {
        txId: "d66c39a24f63d80c13e44cf1ce562618d1d0d92675118aa331e5367a7ddb9de7",
        txOutputN: 1,
        address: "2N3CDv7U6xVYmNqdvNscKBWwUYky7SM6Wdq",
        value: BTC.fromSatoshis(5000L),
        nCoSigners: 1,
        confirmations: 6,
        coordinates: (
          AccountIndex.first,
          AccountKeyChainIndex.first,
          ChainIndex.externalChain,
          AddressIndex.first |> AddressIndex.next
        )
      }
    ];
    let changeAddress =
      AccountKeyChain.Address.{
        nCoSigners: 1,
        coordinates: (
          AccountIndex.first,
          AccountKeyChainIndex.first,
          ChainIndex.externalChain,
          AddressIndex.first
        ),
        witnessScript: "51210358ebee38e86598266dc351dfec81c0bd98e3a90a4e93bff72003569f2b02d13351ae",
        redeemScript: "002027fa0596838478a59b5c0512acf480fdba510cd320def9e3d9e9d27a13b7e72f",
        address: "2N3gWQwj2RrHaw7rWmbr1vKkzBnutSMp2LE"
      };
    test("uses as many inputs as necessary", () => {
      let (payoutTx, changeUsed) =
        PayoutTransaction.build(
          ~mandatoryInputs=[],
          ~allInputs=inputs,
          ~destinations=[
            ("mgWUuj1J1N882jmqFxtDepEC73Rr22E9GU", BTC.fromSatoshis(10000L))
          ],
          ~satsPerByte=BTC.fromSatoshis(1L),
          ~changeAddress,
          ~network=Network.Regtest.network
        );
      expect((payoutTx.usedInputs |> List.length, changeUsed))
      |> toEqual((2, true));
    });
    test("uses smallest possible input", () => {
      let (payoutTx, changeUsed) =
        PayoutTransaction.build(
          ~mandatoryInputs=[],
          ~allInputs=inputs,
          ~destinations=[
            ("mgWUuj1J1N882jmqFxtDepEC73Rr22E9GU", BTC.fromSatoshis(4000L))
          ],
          ~satsPerByte=BTC.fromSatoshis(1L),
          ~changeAddress,
          ~network=Network.Regtest.network
        );
      expect((snd(payoutTx.usedInputs |> List.hd).txOutputN, changeUsed))
      |> toEqual((1, true));
    });
    test("doesn't use change address if not worth it", () => {
      let (payoutTx, changeUsed) =
        PayoutTransaction.build(
          ~mandatoryInputs=[],
          ~allInputs=inputs,
          ~destinations=[
            ("mgWUuj1J1N882jmqFxtDepEC73Rr22E9GU", BTC.fromSatoshis(9800L))
          ],
          ~satsPerByte=BTC.fromSatoshis(1L),
          ~changeAddress,
          ~network=Network.Regtest.network
        );
      expect((snd(payoutTx.usedInputs |> List.hd).txOutputN, changeUsed))
      |> toEqual((0, false));
    });
    test("respects mandatory inputs", () => {
      let (payoutTx, changeUsed) =
        PayoutTransaction.build(
          ~mandatoryInputs=[List.nth(inputs, 1)],
          ~allInputs=inputs,
          ~destinations=[
            ("mgWUuj1J1N882jmqFxtDepEC73Rr22E9GU", BTC.fromSatoshis(6000L))
          ],
          ~satsPerByte=BTC.fromSatoshis(1L),
          ~changeAddress,
          ~network=Network.Regtest.network
        );
      expect((payoutTx.usedInputs |> List.length, changeUsed))
      |> toEqual((2, true));
    });
    test("raises when there aren't enough funds", () =>
      expectFn(
        () =>
          PayoutTransaction.build(
            ~mandatoryInputs=[],
            ~allInputs=inputs,
            ~destinations=[
              ("mgWUuj1J1N882jmqFxtDepEC73Rr22E9GU", BTC.fromSatoshis(15000L))
            ],
            ~satsPerByte=BTC.fromSatoshis(1L),
            ~changeAddress,
            ~network=Network.Regtest.network
          ),
        ()
      )
      |> toThrow
    );
  });
  Skip.describe("build2", () => {
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
    let firstAddressCoordinates =
      AccountKeyChain.Address.Coordinates.firstExternal(accountKeyChain1);
    let secondAddressCoordinates =
      AccountKeyChain.Address.Coordinates.next(firstAddressCoordinates);
    let firstAddress =
      AccountKeyChain.Address.make(firstAddressCoordinates, accountKeyChain1);
    let secondAddress =
      AccountKeyChain.Address.make(secondAddressCoordinates, accountKeyChain1);
    let changeAddressCoordinates =
      AccountKeyChain.Address.Coordinates.firstInternal(accountKeyChain1);
    let changeAddress =
      AccountKeyChain.Address.make(changeAddressCoordinates, accountKeyChain1);
    Skip.testPromise(~timeout=10000, "hello", () =>
      Js.Promise.(
        Helpers.faucet([
          (firstAddress.address, BTC.fromSatoshis(10000L)),
          (secondAddress.address, BTC.fromSatoshis(10000L))
        ])
        |> then_((_) =>
             Network.Regtest.getTransactionInputs(
               [firstAddressCoordinates, secondAddressCoordinates],
               [(accountIdx, [(accountKeyChainIdx, accountKeyChain1)])]
             )
           )
        |> then_(inputs => {
             let (payoutTx, used) =
               PayoutTransaction.build(
                 ~mandatoryInputs=[],
                 ~allInputs=inputs,
                 ~destinations=[
                   (
                     "mgWUuj1J1N882jmqFxtDepEC73Rr22E9GU",
                     BTC.fromSatoshis(9700L)
                   )
                 ],
                 ~satsPerByte=BTC.fromSatoshis(1L),
                 ~changeAddress,
                 ~network=Network.Regtest.network
               );
             expect((payoutTx.usedInputs |> List.length, used))
             |> toEqual((1, false))
             |> resolve;
           })
      )
    );
  });
};
