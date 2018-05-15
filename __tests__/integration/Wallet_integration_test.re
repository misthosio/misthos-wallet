Helpers.enableHttpRequests();

open Jest;

open Expect;

open WalletTypes;

open Event;

open WalletHelpers;

let () =
  describe("integration", () => {
    let (userA, userB, userC) = G.threeUserSessions();
    let log =
      L.(
        createVenture(userA)
        |> withFirstPartner(userA)
        |> withAccount(~supporter=userA)
        |> withCustodian(userA, ~supporters=[userA])
        |> withPartner(userB, ~supporters=[userA])
        |> withCustodian(userB, ~supporters=[userA, userB])
        |> withCustodianKeyChain(userA)
        |> withCustodianKeyChain(userB)
        |> withAccountKeyChainIdentified
        |> withAccountKeyChainActivated(userA)
        |> withAccountKeyChainActivated(userB)
      );
    let accountIdx = AccountIndex.default;
    let ventureId = log |> L.ventureId;
    let wallet = log |> constructState;
    let ((address1, address2), (wallet, log)) =
      (wallet, log) |> collectNextTwoAddresses(userA);
    let log =
      L.(
        log
        |> withPartner(userC, ~supporters=[userA, userB])
        |> withCustodian(userC, ~supporters=[userA, userB])
        |> withCustodianKeyChain(userC)
        |> withAccountKeyChainIdentified
        |> withAccountKeyChainActivated(userA)
        |> withAccountKeyChainActivated(userB)
        |> withAccountKeyChainActivated(userC)
      );
    let oneKeyChainWallet = ref(wallet);
    let wallet = log |> constructState;
    let ((address3, address4), (wallet, _)) =
      (wallet, log) |> collectNextTwoAddresses(userC);
    let twoKeyChainWallet = ref(wallet);
    let address1Satoshis = BTC.fromSatoshis(10000L);
    let address2Satoshis = BTC.fromSatoshis(10000L);
    let address3Satoshis = BTC.fromSatoshis(10000L);
    let address4Satoshis = BTC.fromSatoshis(15000L);
    let oneKeyChainWalletTotal =
      address1Satoshis |> BTC.plus(address2Satoshis);
    let oneKeyChainSpendAmount = BTC.fromSatoshis(6000L);
    let misthosFeePercent = 2.9;
    let oneKeyChainExpectedFee =
      BTC.fromSatoshis(1892L)
      |> BTC.plus(
           oneKeyChainSpendAmount
           |> BTC.timesRounded(misthosFeePercent /. 100.),
         );
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
        |> then_(utxos => {
             let walletOneAddresses = [
               (address1.address, address1),
               (address2.address, address2),
             ];
             let walletTwoAddresses = [
               (address1.address, address1),
               (address2.address, address2),
               (address3.address, address3),
               (address4.address, address4),
             ];
             utxos
             |> List.iter(({address, txId, txOutputN, amount}: utxo) => {
                  let incomeEvent =
                    IncomeDetected.make(
                      ~txOutputN,
                      ~address,
                      ~txId,
                      ~amount,
                      ~coordinates=
                        (walletTwoAddresses |> List.assoc(address)).
                          coordinates,
                      ~blockHeight=0.,
                      ~unixTime=0.,
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
                      |> Wallet.apply(IncomeDetected(incomeEvent))
                  };
                });
             oneKeyChainWallet^
             |> Wallet.preparePayoutTx(
                  userA,
                  accountIdx,
                  [(Helpers.faucetAddress, oneKeyChainSpendAmount)],
                  BTC.fromSatoshis(10L),
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
                     PayoutTransaction.finalize(
                       [data.payoutTx],
                       Network.Regtest,
                     )
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
    testPromise(~timeout=80000, "2 of 3 wallet", () =>
      Js.Promise.(
        twoKeyChainWallet^
        |> Wallet.preparePayoutTx(
             userA,
             accountIdx,
             [(Helpers.faucetAddress, twoKeyChainSpendAmount)],
             BTC.fromSatoshis(10L),
           )
        |> (
          fun
          | Wallet.Ok(({data} as event: Event.Payout.Proposed.t)) => {
              let payoutTx =
                PayoutTransaction.signPayout(
                  ~ventureId,
                  ~userId=userB.userId,
                  ~masterKeyChain=userB.masterKeyChain,
                  ~accountKeyChains=wallet.txInputCollector.keyChains,
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
            }
          | Wallet.NotEnoughFunds => raise(PayoutTransaction.NotEnoughFunds)
        )
        |> then_(((wallet, _broadcastResult)) => {
             let expectedFee =
               BTC.fromSatoshis(5810L)
               |> BTC.plus(
                    twoKeyChainSpendAmount
                    |> BTC.timesRounded(misthosFeePercent /. 100.),
                  );
             wallet
             |> WalletHelpers.getExposedAddresses
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
