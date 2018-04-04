Helpers.enableHttpRequests();

open Jest;

open Expect;

open PrimitiveTypes;

open WalletTypes;

open Bitcoin;

open Event;

module Wallet = Venture__Wallet;

let () =
  describe("interation", () => {
    let (userA, userB) = (
      UserId.fromString("userA"),
      UserId.fromString("userB")
    );
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
    let (masterA, masterB) = (
      HDNode.make(keyA, chainCode),
      HDNode.make(keyB, chainCode)
    );
    let ventureId = VentureId.fromString("test");
    let accountIdx = AccountIndex.default;
    let keyChainIdx = CustodianKeyChainIndex.first;
    let (cKeyChainA, cKeyChainB) = (
      CustodianKeyChain.make(
        ~ventureId,
        ~accountIdx,
        ~keyChainIdx,
        ~masterKeyChain=masterA
      )
      |> CustodianKeyChain.toPublicKeyChain,
      CustodianKeyChain.make(
        ~ventureId,
        ~accountIdx,
        ~keyChainIdx,
        ~masterKeyChain=masterB
      )
      |> CustodianKeyChain.toPublicKeyChain
    );
    testPromise(
      ~timeout=20000,
      "1 of 2 wallet",
      () => {
        let accountKeyChain =
          AccountKeyChain.make(
            accountIdx,
            AccountKeyChainIndex.first,
            1,
            [(userA, cKeyChainA), (userB, cKeyChainB)]
          );
        let wallet =
          Wallet.make()
          |> Wallet.apply(
               AccountKeyChainUpdated(
                 AccountKeyChainUpdated.make(~keyChain=accountKeyChain)
               )
             );
        let address1 = wallet |> Wallet.exposeNextIncomeAddress(accountIdx);
        let wallet = wallet |> Wallet.apply(IncomeAddressExposed(address1));
        let address2 = wallet |> Wallet.exposeNextIncomeAddress(accountIdx);
        let wallet = wallet |> Wallet.apply(IncomeAddressExposed(address2));
        Js.Promise.(
          Helpers.faucet([
            (address1.address, BTC.fromSatoshis(10000L)),
            (address2.address, BTC.fromSatoshis(10000L))
          ])
          |> then_((_) =>
               wallet
               |> Wallet.preparePayoutTx(
                    ventureId,
                    Session.Data.{
                      userId: userA,
                      appKeyPair: keyA,
                      address: keyA |> Bitcoin.ECPair.getAddress,
                      masterKeyChain: masterA
                    },
                    accountIdx,
                    [
                      (
                        "mgWUuj1J1N882jmqFxtDepEC73Rr22E9GU",
                        BTC.fromSatoshis(12000L)
                      )
                    ],
                    BTC.fromSatoshis(1L)
                  )
             )
          |> then_(({data}: Event.Payout.Proposal.t) =>
               TxBuilder.fromTransactionWithNetwork(
                 data.payoutTx.txHex |> Transaction.fromHex,
                 Network.Regtest.network
               )
               |> TxBuilder.build
               |> Network.Regtest.broadcastTransaction
             )
          |> then_(res => {
               let completed =
                 switch res {
                 | Ok(_) => true
                 | Error(_) => false
                 };
               expect(completed) |> toEqual(true) |> resolve;
             })
        );
      }
    );
  });
