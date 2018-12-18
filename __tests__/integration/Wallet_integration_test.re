Helpers.enableHttpRequests();

open Jest;

open Expect;

open PrimitiveTypes;

open WalletTypes;

open Event;

open WalletHelpers;

let testSequence = 4;

let () =
  describe("Wallet_integration", () =>
    describe("integration", () => {
      let (user1, user2, user3) = G.threeUserSessions();
      let log =
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
          |> withCustodian(user1, ~supporters=[user1])
          |> withPartner(user2, ~supporters=[user1])
          |> withCustodian(user2, ~supporters=[user1, user2])
          |> withCustodianKeyChain(user1)
          |> withCustodianKeyChain(user2)
          |> withAccountKeyChainIdentified(~sequence=testSequence)
          |> withAccountKeyChainActivated(user1)
          |> withAccountKeyChainActivated(user2)
        );
      let accountIdx = AccountIndex.default;
      let ventureId = log |> L.ventureId;
      let wallet = log |> constructState;
      let ((address1, address2), (wallet, log)) =
        (wallet, log) |> collectNextTwoAddresses(user1);
      let log =
        L.(
          log
          |> withPartner(user3, ~supporters=[user1, user2])
          |> withCustodian(user3, ~supporters=[user1, user2])
          |> withCustodianKeyChain(user3)
          |> withAccountKeyChainIdentified(~sequence=testSequence)
          |> withAccountKeyChainActivated(user1)
          |> withAccountKeyChainActivated(user2)
          |> withAccountKeyChainActivated(user3)
        );
      let oneKeyChainWallet = ref(wallet);
      let wallet = log |> constructState;
      let ((address3, address4), (wallet, _)) =
        (wallet, log) |> collectNextTwoAddresses(user3);
      let twoKeyChainWallet = ref(wallet);
      let address1Satoshis = BTC.fromSatoshis(10000L);
      let address2Satoshis = BTC.fromSatoshis(10000L);
      let address3Satoshis = BTC.fromSatoshis(10000L);
      let address4Satoshis = BTC.fromSatoshis(15000L);
      let oneKeyChainWalletTotal =
        address1Satoshis |> BTC.plus(address2Satoshis);
      let oneKeyChainSpendAmount = BTC.fromSatoshis(6100L);
      let oneKeyChainExpectedFee = BTC.fromSatoshis(1892L);
      let twoKeyChainWalletTotal =
        (
          oneKeyChainWalletTotal
          |> BTC.plus(address3Satoshis)
          |> BTC.plus(address4Satoshis)
        )
        ->(BTC.minus(oneKeyChainSpendAmount))
        ->(BTC.minus(oneKeyChainExpectedFee));
      let twoKeyChainSpendAmount = BTC.fromSatoshis(25000L);

      beforeAllPromise(~timeout=40000, () =>
        Js.Promise.(
          Helpers.faucet([
            (address1.address.displayAddress, address1Satoshis),
            (address2.address.displayAddress, address2Satoshis),
            (address3.address.displayAddress, address3Satoshis),
            (address4.address.displayAddress, address4Satoshis),
          ])
          |> then_(utxos => {
               let utxos = utxos->Belt.Set.toList;
               let walletOneAddresses = [
                 (address1.address.displayAddress, address1),
                 (address2.address.displayAddress, address2),
               ];
               let walletTwoAddresses = [
                 (address1.address.displayAddress, address1),
                 (address2.address.displayAddress, address2),
                 (address3.address.displayAddress, address3),
                 (address4.address.displayAddress, address4),
               ];
               utxos
               |> List.iter(({address, txId, txOutputN, amount}: utxo) => {
                    let incomeEvent =
                      Income.Detected.make(
                        ~txOutputN,
                        ~address,
                        ~txId,
                        ~amount,
                        ~coordinates=
                          (walletTwoAddresses |> List.assoc(address)).address.
                            coordinates,
                      );
                    switch (walletOneAddresses |> List.mem_assoc(address)) {
                    | true =>
                      oneKeyChainWallet :=
                        oneKeyChainWallet^
                        |> Wallet.apply(IncomeDetected(incomeEvent));
                      twoKeyChainWallet :=
                        twoKeyChainWallet^
                        |> Wallet.apply(IncomeDetected(incomeEvent));
                    | _ =>
                      twoKeyChainWallet :=
                        twoKeyChainWallet^
                        |> Wallet.apply(IncomeDetected(incomeEvent));
                      if (address == address3.address.displayAddress) {
                        twoKeyChainWallet :=
                          twoKeyChainWallet^
                          |> Wallet.apply(
                               IncomeUnlocked(
                                 Income.Unlocked.make(
                                   ~input={
                                     txId,
                                     txOutputN,
                                     address,
                                     value: amount,
                                     nCoSigners: address3.address.nCoSigners,
                                     nPubKeys: address3.address.nPubKeys,
                                     sequence: address3.address.sequence,
                                     coordinates: address3.address.coordinates,
                                     unlocked: true,
                                   },
                                 ),
                               ),
                             );
                      };
                    };
                  });
               let walletInfoCollector =
                 oneKeyChainWallet^.walletInfoCollector;
               let mandatoryInputs =
                 walletInfoCollector
                 |> WalletInfoCollector.oldSpendableInputs(
                      AccountIndex.default,
                    );
               let unlockedInputs =
                 walletInfoCollector
                 |> WalletInfoCollector.unlockedInputs(AccountIndex.default);
               let optionalInputs =
                 walletInfoCollector
                 |> WalletInfoCollector.currentSpendableInputs(
                      AccountIndex.default,
                    );
               let payoutTx =
                 PayoutTransaction.build(
                   ~mandatoryInputs,
                   ~unlockedInputs,
                   ~optionalInputs,
                   ~destinations=[
                     (Helpers.faucetAddress, oneKeyChainSpendAmount),
                   ],
                   ~satsPerByte=BTC.fromSatoshis(10L),
                   ~changeAddress=
                     walletInfoCollector
                     |> WalletInfoCollector.nextChangeAddress(
                          AccountIndex.default,
                          user1.userId,
                        ),
                   ~network=Network.Testnet,
                 );
               oneKeyChainWallet^
               |> Wallet.preparePayoutTx(
                    ~eligibleWhenProposing=
                      [|user1.userId, user2.userId|]
                      |> Belt.Set.mergeMany(UserId.emptySet),
                    user1,
                    accountIdx,
                    payoutTx,
                    [||],
                  )
               |> (
                 fun
                 | Wallet.Ok(
                     ({data, processId} as event: Event.Payout.Proposed.t),
                   ) => {
                     oneKeyChainWallet :=
                       oneKeyChainWallet^
                       |> Wallet.apply(PayoutProposed(event));
                     twoKeyChainWallet :=
                       twoKeyChainWallet^
                       |> Wallet.apply(PayoutProposed(event));
                     all2((
                       processId |> resolve,
                       PayoutTransaction.finalize([data.payoutTx])
                       |> Helpers.broadcastTransaction,
                     ));
                   }
                 | Wallet.NotEnoughFunds =>
                   raise(PayoutTransaction.NotEnoughFunds)
               );
             })
          |> then_(((processId, txId)) => {
               oneKeyChainWallet :=
                 oneKeyChainWallet^
                 |> Wallet.apply(
                      PayoutBroadcast(
                        Payout.Broadcast.make(~processId, ~txId),
                      ),
                    );
               twoKeyChainWallet :=
                 twoKeyChainWallet^
                 |> Wallet.apply(
                      PayoutBroadcast(
                        Payout.Broadcast.make(~processId, ~txId),
                      ),
                    );
               resolve();
             })
        )
      );
      testPromise("1 of 2 wallet", () =>
        Js.Promise.(
          oneKeyChainWallet^
          |> WalletHelpers.getExposedAddresses
          |> Helpers.getUTXOs
          |> then_(utxos =>
               utxos->Belt.Set.toList
               |> List.fold_left(
                    (total, utxo: WalletTypes.utxo) =>
                      total |> BTC.plus(utxo.amount),
                    BTC.zero,
                  )
               |> expect
               |> toEqual(
                    oneKeyChainWalletTotal
                    ->(BTC.minus(oneKeyChainSpendAmount))
                    ->(BTC.minus(oneKeyChainExpectedFee)),
                  )
               |> resolve
             )
        )
      );
      testPromise(
        ~timeout=80000,
        "2 of 3 wallet",
        () => {
          Helpers.genBlocks(testSequence);
          let walletInfoCollector = twoKeyChainWallet^.walletInfoCollector;
          let mandatoryInputs =
            walletInfoCollector
            |> WalletInfoCollector.oldSpendableInputs(AccountIndex.default);
          let unlockedInputs =
            walletInfoCollector
            |> WalletInfoCollector.unlockedInputs(AccountIndex.default);
          let optionalInputs =
            walletInfoCollector
            |> WalletInfoCollector.currentSpendableInputs(
                 AccountIndex.default,
               );
          let payoutTx =
            PayoutTransaction.build(
              ~mandatoryInputs,
              ~unlockedInputs,
              ~optionalInputs,
              ~destinations=[
                (Helpers.faucetAddress, twoKeyChainSpendAmount),
              ],
              ~satsPerByte=BTC.fromSatoshis(10L),
              ~changeAddress=
                walletInfoCollector
                |> WalletInfoCollector.nextChangeAddress(
                     AccountIndex.default,
                     user1.userId,
                   ),
              ~network=Network.Testnet,
            );
          Js.Promise.(
            twoKeyChainWallet^
            |> Wallet.preparePayoutTx(
                 ~eligibleWhenProposing=
                   [|user1.userId, user2.userId, user3.userId|]
                   |> Belt.Set.mergeMany(UserId.emptySet),
                 user1,
                 accountIdx,
                 payoutTx,
                 [||],
               )
            |> (
              fun
              | Wallet.Ok(({data} as event: Event.Payout.Proposed.t)) => {
                  let payoutTx =
                    PayoutTransaction.signPayout(
                      ~ventureId,
                      ~userId=user2.userId,
                      ~masterKeyChain=user2.masterKeyChain,
                      ~accountKeyChains=
                        wallet.walletInfoCollector
                        |> WalletInfoCollector.accountKeyChains,
                      ~payoutTx=data.payoutTx,
                      ~signatures=[||],
                    )
                    |> PayoutTransaction.getSignedExn;
                  Js.Promise.all2((
                    resolve(
                      twoKeyChainWallet^
                      |> Wallet.apply(PayoutProposed(event)),
                    ),
                    PayoutTransaction.finalize([data.payoutTx, payoutTx])
                    |> Helpers.broadcastTransaction,
                  ));
                }
              | Wallet.NotEnoughFunds =>
                raise(PayoutTransaction.NotEnoughFunds)
            )
            |> then_(((wallet, _broadcastResult)) => {
                 let expectedFee = BTC.fromSatoshis(5687L);
                 wallet
                 |> WalletHelpers.getExposedAddresses
                 |> Helpers.getUTXOs
                 |> then_(utxos =>
                      utxos->Belt.Set.toList
                      |> List.fold_left(
                           (total, utxo: WalletTypes.utxo) =>
                             total |> BTC.plus(utxo.amount),
                           BTC.zero,
                         )
                      |> expect
                      |> toEqual(
                           twoKeyChainWalletTotal
                           ->(BTC.minus(twoKeyChainSpendAmount))
                           ->(BTC.minus(expectedFee)),
                         )
                      |> resolve
                    );
               })
          );
        },
      );
    })
  );
