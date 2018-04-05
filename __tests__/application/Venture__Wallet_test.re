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
    let createdEvent =
      VentureCreated.make(
        ~ventureName="test",
        ~creatorId=userA,
        ~creatorPubKey=keyA |> Utils.publicKeyFromKeyPair,
        ~metaPolicy=Policy.absolute
      );
    let chainCode =
      "c8bce5e6dac6f931af17863878cce2ca3b704c61b3d775fe56881cc8ff3ab1cb"
      |> Utils.bufFromHex;
    let (masterA, masterB) = (
      HDNode.make(keyA, chainCode),
      HDNode.make(keyB, chainCode)
    );
    let ventureId = createdEvent.ventureId;
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
    let accountKeyChain =
      AccountKeyChain.make(
        accountIdx,
        AccountKeyChainIndex.first,
        1,
        [(userA, cKeyChainA), (userB, cKeyChainB)]
      );
    let wallet =
      Wallet.make()
      |> Wallet.apply(VentureCreated(createdEvent))
      |> Wallet.apply(
           AccountKeyChainUpdated(
             AccountKeyChainUpdated.make(~keyChain=accountKeyChain)
           )
         );
    let address1 = wallet |> Wallet.exposeNextIncomeAddress(accountIdx);
    let wallet = wallet |> Wallet.apply(IncomeAddressExposed(address1));
    let address2 = wallet |> Wallet.exposeNextIncomeAddress(accountIdx);
    let wallet = wallet |> Wallet.apply(IncomeAddressExposed(address2));
    let oneKeyChainWallet = wallet;
    let accountKeyChain =
      AccountKeyChain.make(
        accountIdx,
        AccountKeyChainIndex.first |> AccountKeyChainIndex.next,
        2,
        [(userA, cKeyChainA), (userB, cKeyChainB)]
      );
    let wallet =
      wallet
      |> Wallet.apply(
           AccountKeyChainUpdated(
             AccountKeyChainUpdated.make(~keyChain=accountKeyChain)
           )
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
    beforeAllPromise(() =>
      Helpers.faucet([
        (address1.address, address1Satoshis),
        (address2.address, address2Satoshis),
        (address3.address, address3Satoshis),
        (address4.address, address4Satoshis)
      ])
    );
    testPromise(~timeout=20000, "1 of 2 wallet", () =>
      Js.Promise.(
        oneKeyChainWallet
        |> Wallet.preparePayoutTx(
             Session.Data.{
               userId: userA,
               appKeyPair: keyA,
               address: keyA |> Bitcoin.ECPair.getAddress,
               masterKeyChain: masterA
             },
             accountIdx,
             [(Helpers.faucetAddress, oneKeyChainSpendAmount)],
             BTC.fromSatoshis(1L)
           )
        |> then_(({data} as event: Event.Payout.Proposal.t) => {
             twoKeyChainWallet :=
               twoKeyChainWallet^ |> Wallet.apply(PayoutProposed(event));
             Js.Promise.(
               all2((
                 resolve(
                   oneKeyChainWallet |> Wallet.apply(PayoutProposed(event))
                 ),
                 PayoutTransaction.finalize(
                   [data.payoutTx],
                   Network.Regtest.network
                 )
                 |> Helpers.broadcastTransaction
               ))
             );
           })
        |> then_(((wallet, _broadcastResult)) =>
             wallet |> Wallet.balance(accountIdx)
           )
        |> then_(((total, reserved)) =>
             expect((total, reserved))
             |> toEqual((
                  oneKeyChainWalletTotal
                  |> BTC.minus(oneKeyChainSpendAmount)
                  |> BTC.minus(oneKeyChainExpectedFee),
                  BTC.zero
                ))
             |> resolve
           )
      )
    );
    testPromise(~timeout=60000, "2 of 2 wallet", () =>
      Js.Promise.(
        twoKeyChainWallet^
        |> Wallet.preparePayoutTx(
             Session.Data.{
               userId: userA,
               appKeyPair: keyA,
               address: keyA |> Bitcoin.ECPair.getAddress,
               masterKeyChain: masterA
             },
             accountIdx,
             [(Helpers.faucetAddress, twoKeyChainSpendAmount)],
             BTC.fromSatoshis(1L)
           )
        |> then_(({data} as event: Event.Payout.Proposal.t) => {
             let PayoutTransaction.Signed(payoutTx) =
               PayoutTransaction.signPayout(
                 ~ventureId,
                 ~userId=userB,
                 ~masterKeyChain=masterB,
                 ~accountKeyChains=wallet.accountKeyChains,
                 ~payoutTx=data.payoutTx,
                 ~network=Network.Regtest.network
               );
             Js.Promise.(
               all2((
                 resolve(
                   twoKeyChainWallet^ |> Wallet.apply(PayoutProposed(event))
                 ),
                 PayoutTransaction.finalize(
                   [data.payoutTx, payoutTx],
                   Network.Regtest.network
                 )
                 |> Helpers.broadcastTransaction
               ))
             );
           })
        |> then_(((wallet, _broadcastResult)) =>
             wallet |> Wallet.balance(accountIdx)
           )
        |> then_(((total, reserved)) => {
             let expectedFee = BTC.fromSatoshis(597L);
             expect((total, reserved))
             |> toEqual((
                  twoKeyChainWalletTotal
                  |> BTC.minus(twoKeyChainSpendAmount)
                  |> BTC.minus(expectedFee),
                  BTC.zero
                ))
             |> resolve;
           })
      )
    );
  });
