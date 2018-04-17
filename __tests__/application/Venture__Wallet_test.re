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
      UserId.fromString("userB"),
    );
    let (keyA, keyB) = (
      ECPair.fromWIFWithNetwork(
        "cUVTgxrs44T7zVon5dSDicBkBRjyfLwL7RF1RvR7n94ar3HEaLs1",
        Networks.testnet,
      ),
      ECPair.fromWIFWithNetwork(
        "cPfdeLvhwvAVRRM5wiEWopWviGG65gbxQCHdtFL56PYUJXsTYixf",
        Networks.testnet,
      ),
    );
    let createdEvent =
      VentureCreated.make(
        ~ventureName="test",
        ~creatorId=userA,
        ~creatorPubKey=keyA |> Utils.publicKeyFromKeyPair,
        ~metaPolicy=Policy.absolute,
        ~network=Network.Regtest,
      );
    let chainCode =
      "c8bce5e6dac6f931af17863878cce2ca3b704c61b3d775fe56881cc8ff3ab1cb"
      |> Utils.bufFromHex;
    let (masterA, masterB) = (
      HDNode.make(keyA, chainCode),
      HDNode.make(keyB, chainCode),
    );
    let ventureId = createdEvent.ventureId;
    let accountIdx = AccountIndex.default;
    let keyChainIdx = CustodianKeyChainIndex.first;
    let (cKeyChainA, cKeyChainB) = (
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
    );
    let accountKeyChain =
      AccountKeyChain.make(
        accountIdx,
        AccountKeyChainIndex.first,
        1,
        [(userA, cKeyChainA), (userB, cKeyChainB)],
      );
    let wallet =
      Wallet.make()
      |> Wallet.apply(VentureCreated(createdEvent))
      |> Wallet.apply(
           AccountCreationAccepted({
             dependsOn: None,
             processId: ProcessId.make(),
             data: {
               accountIdx,
               name: "default",
             },
           }),
         )
      |> Wallet.apply(
           AccountKeyChainUpdated(
             AccountKeyChainUpdated.make(~keyChain=accountKeyChain),
           ),
         );
    let address1 = wallet |> Wallet.exposeNextIncomeAddress(accountIdx);
    let wallet = wallet |> Wallet.apply(IncomeAddressExposed(address1));
    let address2 = wallet |> Wallet.exposeNextIncomeAddress(accountIdx);
    let wallet = wallet |> Wallet.apply(IncomeAddressExposed(address2));
    let oneKeyChainWallet = ref(wallet);
    let accountKeyChain =
      AccountKeyChain.make(
        accountIdx,
        AccountKeyChainIndex.first |> AccountKeyChainIndex.next,
        2,
        [(userA, cKeyChainA), (userB, cKeyChainB)],
      );
    let wallet =
      wallet
      |> Wallet.apply(
           AccountKeyChainUpdated(
             AccountKeyChainUpdated.make(~keyChain=accountKeyChain),
           ),
         );
    let address3 = wallet |> Wallet.exposeNextIncomeAddress(accountIdx);
    let wallet = wallet |> Wallet.apply(IncomeAddressExposed(address3));
    let address4 = wallet |> Wallet.exposeNextIncomeAddress(accountIdx);
    let wallet = wallet |> Wallet.apply(IncomeAddressExposed(address4));
    let twoKeyChainWallet = ref(wallet);
    let address1Satoshis = BTC.fromSatoshis(10000L);
    let address2Satoshis = BTC.fromSatoshis(10000L);
    let address3Satoshis = BTC.fromSatoshis(10000L);
    let address4Satoshis = BTC.fromSatoshis(10000L);
    let oneKeyChainWalletTotal =
      address1Satoshis |> BTC.plus(address2Satoshis);
    let oneKeyChainSpendAmount = BTC.fromSatoshis(8000L);
    let oneKeyChainExpectedFee = BTC.fromSatoshis(197L);
    let twoKeyChainWalletTotal =
      oneKeyChainWalletTotal
      |> BTC.plus(address3Satoshis)
      |> BTC.plus(address4Satoshis)
      |> BTC.minus(oneKeyChainSpendAmount)
      |> BTC.minus(oneKeyChainExpectedFee);
    let twoKeyChainSpendAmount = BTC.fromSatoshis(25000L);
    beforeAllPromise(~timeout=40000, () =>
      Js.Promise.(
        Helpers.faucet([
          (address1.address, address1Satoshis),
          (address2.address, address2Satoshis),
          (address3.address, address3Satoshis),
          (address4.address, address4Satoshis),
        ])
        |> then_((_) =>
             oneKeyChainWallet^
             |> Wallet.preparePayoutTx(
                  Session.Data.{
                    appPrivateKey: "",
                    userId: userA,
                    issuerKeyPair: keyA,
                    storagePrefix: keyA |> Bitcoin.ECPair.getAddress,
                    masterKeyChain: masterA,
                    network: Regtest,
                  },
                  accountIdx,
                  [(Helpers.faucetAddress, oneKeyChainSpendAmount)],
                  BTC.fromSatoshis(1L),
                )
           )
        |> then_(({data, processId} as event: Event.Payout.Proposed.t) => {
             oneKeyChainWallet :=
               oneKeyChainWallet^ |> Wallet.apply(PayoutProposed(event));
             twoKeyChainWallet :=
               twoKeyChainWallet^ |> Wallet.apply(PayoutProposed(event));
             all2((
               processId |> resolve,
               PayoutTransaction.finalize([data.payoutTx], Network.Regtest)
               |> Helpers.broadcastTransaction,
             ));
           })
        |> then_(((processId, txId)) => {
             oneKeyChainWallet :=
               oneKeyChainWallet^
               |> Wallet.apply(
                    PayoutBroadcast(
                      Payout.Broadcast.make(~processId, ~transactionId=txId),
                    ),
                  );
             twoKeyChainWallet :=
               twoKeyChainWallet^
               |> Wallet.apply(
                    PayoutBroadcast(
                      Payout.Broadcast.make(~processId, ~transactionId=txId),
                    ),
                  );
             resolve();
           })
      )
    );
    testPromise("1 of 2 wallet", () =>
      Js.Promise.(
        oneKeyChainWallet^
        |> Wallet.getExposedAddresses(~includeChangeAddresses=true)
        |> Helpers.getUTXOs
        |> then_(utxos =>
             utxos
             |> List.fold_left(
                  (total, utxo: WalletTypes.utxo) =>
                    total |> BTC.plus(utxo.amount),
                  BTC.zero,
                )
             |> expect
             |> toEqual(
                  oneKeyChainWalletTotal
                  |> BTC.minus(oneKeyChainSpendAmount)
                  |> BTC.minus(oneKeyChainExpectedFee),
                )
             |> resolve
           )
      )
    );
    testPromise(~timeout=80000, "2 of 2 wallet", () =>
      Js.Promise.(
        twoKeyChainWallet^
        |> Wallet.preparePayoutTx(
             Session.Data.{
               appPrivateKey: "",
               network: Regtest,
               userId: userA,
               issuerKeyPair: keyA,
               storagePrefix: keyA |> Bitcoin.ECPair.getAddress,
               masterKeyChain: masterA,
             },
             accountIdx,
             [(Helpers.faucetAddress, twoKeyChainSpendAmount)],
             BTC.fromSatoshis(1L),
           )
        |> then_(({data} as event: Event.Payout.Proposed.t) => {
             let payoutTx =
               PayoutTransaction.signPayout(
                 ~ventureId,
                 ~userId=userB,
                 ~masterKeyChain=masterB,
                 ~accountKeyChains=wallet.accountKeyChains,
                 ~payoutTx=data.payoutTx,
                 ~network=Network.Regtest,
               )
               |> PayoutTransaction.getSignedExn;
             Js.Promise.all2((
               resolve(
                 twoKeyChainWallet^ |> Wallet.apply(PayoutProposed(event)),
               ),
               PayoutTransaction.finalize(
                 [data.payoutTx, payoutTx],
                 Network.Regtest,
               )
               |> Helpers.broadcastTransaction,
             ));
           })
        |> then_(((wallet, _broadcastResult)) => {
             let expectedFee = BTC.fromSatoshis(597L);
             wallet
             |> Wallet.getExposedAddresses(~includeChangeAddresses=true)
             |> Helpers.getUTXOs
             |> then_(utxos =>
                  utxos
                  |> List.fold_left(
                       (total, utxo: WalletTypes.utxo) =>
                         total |> BTC.plus(utxo.amount),
                       BTC.zero,
                     )
                  |> expect
                  |> toEqual(
                       twoKeyChainWalletTotal
                       |> BTC.minus(twoKeyChainSpendAmount)
                       |> BTC.minus(expectedFee),
                     )
                  |> resolve
                );
           })
      )
    );
  });
