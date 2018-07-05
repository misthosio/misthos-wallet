open Jest;

open Expect;

open Belt;

open PrimitiveTypes;
open WalletTypes;

open WalletHelpers;

let constructState = log =>
  log
  |> L.reduce(
       (s, {event}) => s |> WalletInfoCollector.apply(event),
       WalletInfoCollector.make(),
     );

let () = {
  describe("WalletInfoCollector-addressInfo", () =>
    F.withCached(
      ~scope="WalletInfoCollector-addressInfo",
      "classifies addresses",
      () => G.withUserSessions(5),
      sessions => {
        let (user1, user2, user3, user4, user5) =
          G.fiveUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
          |> withCustodian(user1, ~supporters=[user1])
          |> withCustodianKeyChain(user1)
          |> withAccountKeyChainIdentified
          |> withAccountKeyChainActivated(user1)
          |> withIncomeAddressExposed(user1)
          |> withPartner(user2, ~supporters=[user1])
          |> withCustodian(user2, ~supporters=[user1, user2])
          |> withCustodianKeyChain(user2)
          |> withAccountKeyChainIdentified
          |> withAccountKeyChainActivated(user1)
          |> withIncomeAddressExposed(user1)
          |> withPartner(user3, ~supporters=[user1, user2])
          |> withCustodian(user3, ~supporters=[user1, user2, user3])
          |> withCustodianKeyChain(user3)
          |> withAccountKeyChainIdentified
          |> withAccountKeyChainActivated(user1)
          |> withIncomeAddressExposed(user1)
          |> withCustodianRemoved(user1, ~supporters=[user2, user3])
          |> withCustodianKeyChain(~keyChainIdx=1, user2)
          |> withAccountKeyChainIdentified
          |> withAccountKeyChainActivated(user2)
          |> withIncomeAddressExposed(user2)
          |> withCustodianRemoved(user3, ~supporters=[user2])
          |> withCustodianKeyChain(~keyChainIdx=2, user2)
          |> withAccountKeyChainIdentified
          |> withAccountKeyChainActivated(user2)
          |> withIncomeAddressExposed(user2)
          |> withPartner(user4, ~supporters=[user2])
          |> withCustodian(user4, ~supporters=[user2, user4])
          |> withCustodianKeyChain(user4)
          |> withAccountKeyChainIdentified
          |> withAccountKeyChainActivated(user2)
          |> withIncomeAddressExposed(user2)
          |> withPartner(user5, ~supporters=[user2])
          |> withCustodian(user5, ~supporters=[user2, user4, user4])
          |> withCustodianKeyChain(user5)
          |> withAccountKeyChainIdentified
          |> withAccountKeyChainActivated(user2)
          |> withIncomeAddressExposed(user2)
        );
      },
      (sessions, log) => {
        let testInfo =
            (custodians, type_, status, info: WalletInfoCollector.addressInfo) =>
          describe(
            "AddressInfo of address " ++ info.address,
            () => {
              test("Custodians are correct", () =>
                expect(
                  info.custodians
                  |> Set.eq(
                       Set.mergeMany(
                         UserId.emptySet,
                         custodians
                         |. Array.mapU((. u: SessionData.t) => u.userId),
                       ),
                     ),
                )
                |> toEqual(true)
              );
              test("addressType, addressStatus are correct", () => {
                open WalletInfoCollector;
                let {addressType, addressStatus} = info;
                expect((addressType, addressStatus))
                |> toEqual((type_, status));
              });
            },
          );
        let (user1, user2, user3, user4, user5) =
          G.fiveUserSessionsFromArray(sessions);
        let info =
          log
          |> constructState
          |> WalletInfoCollector.addressInfos(AccountIndex.default);
        switch (info) {
        | [info0, info1, info2, info3, info4, info5, info6] =>
          info0
          |> testInfo(
               [|user2, user4, user5|],
               Income(user2.userId),
               Accessible,
             );
          info1
          |> testInfo(
               [|user2, user4|],
               Income(user2.userId),
               OutdatedCustodians,
             );
          info2 |> testInfo([|user2|], Income(user2.userId), AtRisk);
          info3 |> testInfo([|user2, user3|], Income(user2.userId), AtRisk);
          info4
          |> testInfo(
               [|user1, user2, user3|],
               Income(user1.userId),
               TemporarilyInaccessible,
             );
          info5 |> testInfo([|user1, user2|], Income(user1.userId), AtRisk);
          info6 |> testInfo([|user1|], Income(user1.userId), Inaccessible);
        | _ => %assert
               "WalletInfoCollector_test"
        };
      },
    )
  );
  describe("WalletInfoCollector", () =>
    F.withCached(
      ~scope="WalletInfoCollector",
      "oldInputs",
      () => G.withUserSessions(3),
      sessions => {
        let (user1, user2) = G.twoUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
          |> withCustodian(user1, ~supporters=[user1])
          |> withCustodianKeyChain(user1)
          |> withAccountKeyChainIdentified
          |> withAccountKeyChainActivated(user1)
          |> withIncomeAddressExposed(user1)
          |> withIncomeDetected(~incomeAddress=0)
          |> withIncomeDetected(~incomeAddress=0)
          |> withPartner(user2, ~supporters=[user1])
          |> withCustodian(user2, ~supporters=[user1, user2])
          |> withCustodianKeyChain(user2)
          |> withAccountKeyChainIdentified
          |> withAccountKeyChainActivated(user1)
          |> withIncomeAddressExposed(user1)
          |> withIncomeDetected(~incomeAddress=1)
          |> withIncomeDetected(~incomeAddress=1)
          |> withIncomeDetected(~incomeAddress=1)
          |> withIncomeDetected(~incomeAddress=1)
          |> withCustodianRemoved(user2, ~supporters=[user1])
          |> withPartnerRemoved(user2, ~supporters=[user1])
          |> withCustodianKeyChain(~keyChainIdx=1, user1)
          |> withAccountKeyChainIdentified
          |> withAccountKeyChainActivated(user1)
          |> withIncomeAddressExposed(user1)
          |> withIncomeDetected(~incomeAddress=2)
          |> withIncomeUnlocked(~income=0)
        );
      },
      (_sessions, log) => {
        let info = log |> constructState;
        test("1 input is unlocked", () =>
          expect(
            info
            |> WalletInfoCollector.unlockedInputs(AccountIndex.default)
            |> Set.size,
          )
          |> toEqual(1)
        );
        test("4 inputs are old", () =>
          expect(
            info
            |> WalletInfoCollector.oldSpendableInputs(AccountIndex.default)
            |> Set.size,
          )
          |> toEqual(4)
        );
        test("3 inputs are current", () =>
          expect(
            info
            |> WalletInfoCollector.currentSpendableInputs(
                 AccountIndex.default,
               )
            |> Set.size,
          )
          |> toEqual(2)
        );
      },
    )
  );
};
