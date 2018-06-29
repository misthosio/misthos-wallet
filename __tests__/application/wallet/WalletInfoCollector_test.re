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
          |> withPartner(user2, ~supporters=[user1])
          |> withCustodian(user2, ~supporters=[user1, user2])
          |> withCustodianKeyChain(user2)
          |> withAccountKeyChainIdentified
          |> withAccountKeyChainActivated(user1)
          |> withIncomeAddressExposed(user1)
          |> withPartnerRemoved(user2, ~supporters=[user1])
          |> withCustodianKeyChain(~keyChainIdx=1, user1)
          |> withAccountKeyChainIdentified
          |> withAccountKeyChainActivated(user1)
          |> withIncomeAddressExposed(user1)
          |> withIncomeDetected(~incomeAddress=0)
          |> withIncomeDetected(~incomeAddress=1)
          |> withIncomeDetected(~incomeAddress=2)
        );
      },
      (sessions, log) => {
        let (user1, _user2) = G.twoUserSessionsFromArray(sessions);
        let info = log |> constructState;
        test("1 input is old", () =>
          expect(
            info
            |> WalletInfoCollector.nonReservedOldInputs(
                 AccountIndex.default,
                 user1.userId,
               )
            |> Set.size,
          )
          |> toEqual(1)
        );
      },
    )
  );
  describe("WalletInfoCollector-addressInfo", () =>
    F.withCached(
      ~scope="WalletInfoCollector-addressInfo",
      "classifies addresses",
      () => F.threeUserSessionsArray,
      sessions => {
        let (user1, user2, user3) = G.threeUserSessionsFromArray(sessions);
        L.(
          F.createVenture(user1)
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
          |> withPartner(user3, ~supporters=[user1, user2])
          |> withCustodian(user3, ~supporters=[user1, user2, user3])
          |> withCustodianKeyChain(user3)
          |> withAccountKeyChainIdentified
          |> withAccountKeyChainActivated(user1)
          |> withIncomeAddressExposed(user1)
          |> withCustodianRemoved(user2, ~supporters=[user1, user3])
          |> withAccountKeyChainIdentified
          |> withAccountKeyChainActivated(user1)
          |> withIncomeAddressExposed(user1)
        );
      },
      (sessions, log) => {
        let (user1, user2, user3) = G.threeUserSessionsFromArray(sessions);
        let info = log |> constructState |> WalletInfoCollector.addressInfos;
        let [lastInfo, secondInfo, firstInfo] = info;
        Skip.describe("AddressInfo of first address", () => {
          test("Custodians are correct", () =>
            expect(
              firstInfo.custodians
              |> Set.eq(Set.mergeMany(UserId.emptySet, [|user1.userId|])),
            )
            |> toEqual(true)
          );
          test("addressType, addressStatus, balance are correct", () => {
            open WalletInfoCollector;
            let {addressType, addressStatus, balance} = firstInfo;
            expect((addressType, addressStatus, balance))
            |> toEqual((Income, OutdatedCustodians, BTC.zero));
          });
        });
        describe("AddressInfo of last address", () => {
          test("Custodians are correct", () =>
            expect(
              lastInfo.custodians
              |> Set.eq(
                   Set.mergeMany(
                     UserId.emptySet,
                     [|user1.userId, user3.userId|],
                   ),
                 ),
            )
            |> toEqual(true)
          );
          test("addressType, addressStatus, balance are correct", () => {
            open WalletInfoCollector;
            let {addressType, addressStatus, balance} = lastInfo;
            expect((addressType, addressStatus, balance))
            |> toEqual((Income, Accessible, BTC.zero));
          });
        });
      },
    )
  );
};
