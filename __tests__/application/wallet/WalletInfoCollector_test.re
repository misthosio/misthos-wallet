open Jest;

open Expect;

open Belt;

open WalletTypes;

open WalletHelpers;

let constructState = log =>
  log
  |> L.reduce(
       (s, {event}) => s |> WalletInfoCollector.apply(event),
       WalletInfoCollector.make(),
     );

let () =
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
        test("2 input is old", () =>
          expect(
            info
            |> WalletInfoCollector.oldInputs(
                 AccountIndex.default,
                 user1.userId,
               )
            |> Set.size,
          )
          |> toEqual(2)
        );
      },
    )
  );
