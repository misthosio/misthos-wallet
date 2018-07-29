Helpers.enableHttpRequests();

open Jest;

open Expect;

open PrimitiveTypes;

open WalletTypes;

open Event;

open WalletHelpers;

let testSequence = 4;

/* let () = */
/* describe("Wallet_integration", () => */
/*   F.withCached( */
/*     ~scope="Wallet_integration", */
/*     "integration", */
/*     () => G.withUserSessions(3), */
/*     sessions => { */
/*       let (user1, user2, _user3) = G.threeUserSessionsFromArray(sessions); */
/*       L.( */
/*         createVenture(user1) */
/*         |> withFirstPartner(user1) */
/*         |> withAccount(~supporter=user1) */
/*         |> withCustodian(user1, ~supporters=[user1]) */
/*         |> withPartner(user2, ~supporters=[user1]) */
/*         |> withCustodian(user2, ~supporters=[user1, user2]) */
/*         |> withCustodianKeyChain(user1) */
/*         |> withCustodianKeyChain(user2) */
/*         |> withAccountKeyChainIdentified(~sequence=testSequence) */
/*         |> withAccountKeyChainActivated(user1) */
/*         |> withAccountKeyChainActivated(user2) */
/*       ); */
/*     }, */
/*     (sessions, log) => { */
/*       let (user1, user2, user3) = G.threeUserSessionsFromArray(sessions); */
/*       let accountIdx = AccountIndex.default; */
/*       let ventureId = log |> L.ventureId; */
/*       let wallet = log |> constructState; */
/*       let ((address1, address2), (wallet, log)) = */
/*         (wallet, log) |> collectNextTwoAddresses(user1); */
/*       let log = */
/*         L.( */
/*           log */
/*           |> withPartner(user3, ~supporters=[user1, user2]) */
/*           |> withCustodian(user3, ~supporters=[user1, user2]) */
/*           |> withCustodianKeyChain(user3) */
/*           |> withAccountKeyChainIdentified(~sequence=testSequence) */
/*           |> withAccountKeyChainActivated(user1) */
/*           |> withAccountKeyChainActivated(user2) */
/*           |> withAccountKeyChainActivated(user3) */
/*         ); */
/*       let oneKeyChainWallet = ref(wallet); */
/*       let wallet = log |> constructState; */
/*       let ((address3, address4), (wallet, _)) = */
/*         (wallet, log) |> collectNextTwoAddresses(user3); */
/*       let twoKeyChainWallet = ref(wallet); */
/*       let address1Satoshis = BTC.fromSatoshis(10000L); */
/*       let address2Satoshis = BTC.fromSatoshis(10000L); */
/*       let address3Satoshis = BTC.fromSatoshis(10000L); */
/*       let address4Satoshis = BTC.fromSatoshis(15000L); */
/*       let oneKeyChainWalletTotal = */
/*         address1Satoshis |> BTC.plus(address2Satoshis); */
/*       let oneKeyChainSpendAmount = BTC.fromSatoshis(6100L); */
/*       let misthosFeePercent = PayoutTransaction.misthosFeePercent; */
/*       let oneKeyChainExpectedFee = */
/*         BTC.fromSatoshis(1892L) */
/*         |> BTC.plus( */
/*              oneKeyChainSpendAmount */
/*              |> BTC.timesRounded(misthosFeePercent /. 100.), */
/*            ); */
/*       let twoKeyChainWalletTotal = */
/*         oneKeyChainWalletTotal */
/*         |> BTC.plus(address3Satoshis) */
/*         |> BTC.plus(address4Satoshis) */
/*         |> BTC.minus(oneKeyChainSpendAmount) */
/*         |> BTC.minus(oneKeyChainExpectedFee); */
/*       let twoKeyChainSpendAmount = BTC.fromSatoshis(25000L); */

/*       beforeAllPromise(~timeout=40000, () => */
/*         Js.Promise.( */
/*           Helpers.faucet([ */
/*             (address1.address.displayAddress, address1Satoshis), */
/*             (address2.address.displayAddress, address2Satoshis), */
/*             (address3.address.displayAddress, address3Satoshis), */
/*             (address4.address.displayAddress, address4Satoshis), */
/*           ]) */
/*           |> then_(utxos => { */
/*                let walletOneAddresses = [ */
/*                  (address1.address.displayAddress, address1), */
/*                  (address2.address.displayAddress, address2), */
/*                ]; */
/*                let walletTwoAddresses = [ */
/*                  (address1.address.displayAddress, address1), */
/*                  (address2.address.displayAddress, address2), */
/*                  (address3.address.displayAddress, address3), */
/*                  (address4.address.displayAddress, address4), */
/*                ]; */
/*                utxos */
/*                |> List.iter(({address, txId, txOutputN, amount}: utxo) => { */
/*                     let incomeEvent = */
/*                       Income.Detected.make( */
/*                         ~txOutputN, */
/*                         ~address, */
/*                         ~txId, */
/*                         ~amount, */
/*                         ~coordinates= */
/*                           (walletTwoAddresses |> List.assoc(address)). */
/*                             address. */
/*                             coordinates, */
/*                       ); */
/*                     switch (walletOneAddresses |> List.mem_assoc(address)) { */
/*                     | true => */
/*                       oneKeyChainWallet := */
/*                         oneKeyChainWallet^ */
/*                         |> Wallet.apply(IncomeDetected(incomeEvent)); */
/*                       twoKeyChainWallet := */
/*                         twoKeyChainWallet^ */
/*                         |> Wallet.apply(IncomeDetected(incomeEvent)); */
/*                     | _ => */
/*                       twoKeyChainWallet := */
/*                         twoKeyChainWallet^ */
/*                         |> Wallet.apply(IncomeDetected(incomeEvent)); */
/*                       if (address == address3.address.displayAddress) { */
/*                         twoKeyChainWallet := */
/*                           twoKeyChainWallet^ */
/*                           |> Wallet.apply( */
/*                                IncomeUnlocked( */
/*                                  Income.Unlocked.make( */
/*                                    ~input={ */
/*                                      txId, */
/*                                      txOutputN, */
/*                                      address, */
/*                                      value: amount, */
/*                                      nCoSigners: address3.address.nCoSigners, */
/*                                      nPubKeys: address3.address.nPubKeys, */
/*                                      sequence: address3.address.sequence, */
/*                                      coordinates: */
/*                                        address3.address.coordinates, */
/*                                      unlocked: true, */
/*                                    }, */
/*                                  ), */
/*                                ), */
/*                              ); */
/*                       }; */
/*                     }; */
/*                   }); */
/*                oneKeyChainWallet^ */
/*                |> Wallet.preparePayoutTx( */
/*                     ~eligibleWhenProposing= */
/*                       [|user1.userId, user2.userId|] */
/*                       |> Belt.Set.mergeMany(UserId.emptySet), */
/*                     user1, */
/*                     accountIdx, */
/*                     [(Helpers.faucetAddress, oneKeyChainSpendAmount)], */
/*                     BTC.fromSatoshis(10L), */
/*                   ) */
/*                |> ( */
/*                  fun */
/*                  | Wallet.Ok( */
/*                      ({data, processId} as event: Event.Payout.Proposed.t), */
/*                    ) => { */
/*                      oneKeyChainWallet := */
/*                        oneKeyChainWallet^ */
/*                        |> Wallet.apply(PayoutProposed(event)); */
/*                      twoKeyChainWallet := */
/*                        twoKeyChainWallet^ */
/*                        |> Wallet.apply(PayoutProposed(event)); */
/*                      all2(( */
/*                        processId |> resolve, */
/*                        PayoutTransaction.finalize([data.payoutTx]) */
/*                        |> Helpers.broadcastTransaction, */
/*                      )); */
/*                    } */
/*                  | Wallet.NotEnoughFunds => */
/*                    raise(PayoutTransaction.NotEnoughFunds) */
/*                ); */
/*              }) */
/*           |> then_(((processId, txId)) => { */
/*                oneKeyChainWallet := */
/*                  oneKeyChainWallet^ */
/*                  |> Wallet.apply( */
/*                       PayoutBroadcast( */
/*                         Payout.Broadcast.make(~processId, ~txId), */
/*                       ), */
/*                     ); */
/*                twoKeyChainWallet := */
/*                  twoKeyChainWallet^ */
/*                  |> Wallet.apply( */
/*                       PayoutBroadcast( */
/*                         Payout.Broadcast.make(~processId, ~txId), */
/*                       ), */
/*                     ); */
/*                resolve(); */
/*              }) */
/*         ) */
/*       ); */
/*       testPromise("1 of 2 wallet", () => */
/*         Js.Promise.( */
/*           oneKeyChainWallet^ */
/*           |> WalletHelpers.getExposedAddresses */
/*           |> Helpers.getUTXOs */
/*           |> then_(utxos => */
/*                utxos */
/*                |> List.fold_left( */
/*                     (total, utxo: WalletTypes.utxo) => */
/*                       total |> BTC.plus(utxo.amount), */
/*                     BTC.zero, */
/*                   ) */
/*                |> expect */
/*                |> toEqual( */
/*                     oneKeyChainWalletTotal */
/*                     |> BTC.minus(oneKeyChainSpendAmount) */
/*                     |> BTC.minus(oneKeyChainExpectedFee), */
/*                   ) */
/*                |> resolve */
/*              ) */
/*         ) */
/*       ); */
/*       testPromise( */
/*         ~timeout=80000, */
/*         "2 of 3 wallet", */
/*         () => { */
/*           Helpers.genBlocks(testSequence); */
/*           Js.Promise.( */
/*             twoKeyChainWallet^ */
/*             |> Wallet.preparePayoutTx( */
/*                  ~eligibleWhenProposing= */
/*                    [|user1.userId, user2.userId, user3.userId|] */
/*                    |> Belt.Set.mergeMany(UserId.emptySet), */
/*                  user1, */
/*                  accountIdx, */
/*                  [(Helpers.faucetAddress, twoKeyChainSpendAmount)], */
/*                  BTC.fromSatoshis(10L), */
/*                ) */
/*             |> ( */
/*               fun */
/*               | Wallet.Ok(({data} as event: Event.Payout.Proposed.t)) => { */
/*                   let payoutTx = */
/*                     PayoutTransaction.signPayout( */
/*                       ~ventureId, */
/*                       ~userId=user2.userId, */
/*                       ~masterKeyChain=user2.masterKeyChain, */
/*                       ~accountKeyChains= */
/*                         wallet.walletInfoCollector */
/*                         |> WalletInfoCollector.accountKeyChains, */
/*                       ~payoutTx=data.payoutTx, */
/*                     ) */
/*                     |> PayoutTransaction.getSignedExn; */
/*                   Js.Promise.all2(( */
/*                     resolve( */
/*                       twoKeyChainWallet^ */
/*                       |> Wallet.apply(PayoutProposed(event)), */
/*                     ), */
/*                     PayoutTransaction.finalize([data.payoutTx, payoutTx]) */
/*                     |> Helpers.broadcastTransaction, */
/*                   )); */
/*                 } */
/*               | Wallet.NotEnoughFunds => */
/*                 raise(PayoutTransaction.NotEnoughFunds) */
/*             ) */
/*             |> then_(((wallet, _broadcastResult)) => { */
/*                  let expectedFee = */
/*                    BTC.fromSatoshis(5687L) */
/*                    |> BTC.plus( */
/*                         twoKeyChainSpendAmount */
/*                         |> BTC.timesRounded(misthosFeePercent /. 100.), */
/*                       ); */
/*                  wallet */
/*                  |> WalletHelpers.getExposedAddresses */
/*                  |> Helpers.getUTXOs */
/*                  |> then_(utxos => */
/*                       utxos */
/*                       |> List.fold_left( */
/*                            (total, utxo: WalletTypes.utxo) => */
/*                              total |> BTC.plus(utxo.amount), */
/*                            BTC.zero, */
/*                          ) */
/*                       |> expect */
/*                       |> toEqual( */
/*                            twoKeyChainWalletTotal */
/*                            |> BTC.minus(twoKeyChainSpendAmount) */
/*                            |> BTC.minus(expectedFee), */
/*                          ) */
/*                       |> resolve */
/*                     ); */
/*                }) */
/*           ); */
/*         }, */
/*       ); */
/*     }, */
/*   ) */
/* ); */
open Bitcoin;
let wordList = "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about";
let node =
  Bip39.mnemonicToSeed(wordList)
  |. HDNode.fromSeedBuffer(Networks.testnet)
  |> HDNode.derivePath("0'/0");
let keyPair1 = (node |> HDNode.derive(0))##keyPair;
Js.log2("key", keyPair1 |> ECPair.getPublicKeyBuffer |> Utils.bufToHex);
let keyPair2 = (node |> HDNode.derive(1))##keyPair;

open Script;

let witnessScript =
  MultisigWithSequence.encode(
    2,
    [|
      keyPair1 |> ECPair.getPublicKeyBuffer,
      keyPair2 |> ECPair.getPublicKeyBuffer,
    |],
    1000,
  );
Js.log(witnessScript |> Utils.bufToHex);
let redeemScript =
  WitnessScriptHash.Output.encode(Crypto.sha256FromBuffer(witnessScript));
let outputScript = ScriptHash.Output.encode(Crypto.hash160(redeemScript));
let displayAddress = Address.fromOutputScript(outputScript, Networks.testnet);

beforeAllPromise(~timeout=40000, () =>
  Js.Promise.(
    Helpers.faucet([(displayAddress, BTC.fromSatoshis(10000L))])
    |> then_(((utxos, _inputTx)) => {
         let utxo: WalletTypes.utxo = utxos |> List.hd;
         Js.log(utxo);
         let txB = TxBuilder.createWithNetwork(Networks.testnet);
         txB |. TxBuilder.setVersion(2);
         txB |> TxBuilder.addInput(utxo.txId, utxo.txOutputN) |> ignore;
         txB |> TxBuilder.addOutput(Helpers.faucetAddress, 5000.) |> ignore;
         /* txB |> TxBuilder.signMultisig(0, keyPair1, ~redeemScript); */
         let txHex = txB |> TxBuilder.buildIncomplete |> Transaction.toHex;
         let tx = TxWrapper.make(txHex);
         let tx =
           (
             tx
             |> TxWrapper.sign(
                  0,
                  keyPair1,
                  ~nCustodians=2,
                  ~redeemScript=redeemScript |> Utils.bufToHex,
                  ~witnessValue=BTC.fromSatoshis(10000L),
                  ~witnessScript=witnessScript |> Utils.bufToHex,
                )
           ).
             tx;
         Js.log(tx |> Transaction.toHex);
         /* Js.log(txHex); */
         /* Helpers.broadcastTransaction(txB |> TxBuilder.build) */
         /* |> then_(_ => Js.log("TOHSENTOSEHNT") |> resolve); */
         resolve();
       })
  )
);
test("bla", () =>
  expect(true) |> toEqual(true)
);

let tx =
  "01000000000101430a222fbb810de66f1d778907ef8d552348695c361ed4eeccbbfa62b052944e00000000232200201fdc1949d40d86a3225c46b01beb5a2973493ae8e7b11e98f14dbbd282ffc2dfffffffff0188130000000000001976a9140ae1441568d0d293764a347b191025c51556cecd88ac040047304402204b90ce9d9843b95f8896047ac69965e4a1fcf2427c17ead3ab27f58e298d0540022075181521691424526e100fecffa28a748c78ec954314783750f93e7770e406cc0100475221026666422d00f1b308fc7527198749f06fedb028b979c09f60d0348ef79c985e41210384257cf895f1ca492bbee5d7485ae0ef479036fdf59e15b92e37970a98d6fe7552ae00000000"
  |> Transaction.fromHex;
let txHash =
  tx
  |. Transaction.hashForWitnessV0(
       0,
       "5221026666422d00f1b308fc7527198749f06fedb028b979c09f60d0348ef79c985e41210384257cf895f1ca492bbee5d7485ae0ef479036fdf59e15b92e37970a98d6fe7552ae"
       |> Utils.bufFromHex,
       10000.,
       Transaction.sighashAll,
     );

let sign =
  "304402205f2d29ac614d4469b1f0e78b4ba002b0e3477b7f18c935da00db6be9d869eaa4022057b1ea6740c253411ca7137e335cf6e23c6771038a5ce79439cf3e18c610706f"
  |> Utils.bufFromHex
  |> ECSignature.fromDER;
Js.log2("SIGNED:", keyPair1 |> ECPair.verify(txHash, sign));
