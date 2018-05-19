open Jest;

open WalletHelpers;

let () =
  describe("Venture__Wallet", () =>
    F.withCached(
      ~scope="Venture__Wallet",
      "nextIncomeAddress",
      () => F.threeUserSessionsArray,
      sessions => {
        let (user1, user2, _user3) = G.threeUserSessionsFromArray(sessions);
        L.(
          Fixtures.createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
          |> withCustodian(user1, ~supporters=[user1])
          |> withPartner(user2, ~supporters=[user1])
          |> withCustodian(user2, ~supporters=[user1, user2])
          |> withCustodianKeyChain(user1)
          |> withCustodianKeyChain(user2)
          |> withAccountKeyChainIdentified
          |> withAccountKeyChainActivated(user1)
          |> withAccountKeyChainActivated(user2)
        );
      },
      (sessions, log) => {
        let (user1, user2, user3) = G.threeUserSessionsFromArray(sessions);
        log
        |> constructState
        |> testNextIncomeAddress(
             user1,
             "2MtvnaAWnfkSBU7NbpSfMRrCKSTgLeRVd93",
           )
        |> testNextIncomeAddress(
             user2,
             "2NGFyACMJG6KJ9Db92n9MM2nNtR4r4tUNFW",
           )
        |> testNextIncomeAddress(
             user1,
             "2NCAxqkK9bhRQWssyrMqJhXS65xmUCWraXT",
           )
        |> ignore;
        let log =
          L.(
            log
            |> withPartner(user3, ~supporters=[user1, user2])
            |> withCustodian(user3, ~supporters=[user1, user2, user3])
            |> withCustodianKeyChain(user3)
            |> withAccountKeyChainIdentified
            |> withAccountKeyChainActivated(user1)
            |> withAccountKeyChainActivated(user2)
            |> withAccountKeyChainActivated(user3)
          );
        log
        |> constructState
        |> testNextIncomeAddress(
             user3,
             "2NCGfPo6ehd2cgwFNE2ocqUFpv8rtcN3TGj",
           )
        |> testNextIncomeAddress(
             user2,
             "2N1sd2funBMd3ntLSbJrALAz3CJxEVsAPV7",
           )
        |> testNextIncomeAddress(
             user3,
             "2N85sud6RgkaAEPitqdrNXsMbADYzXCWc7T",
           )
        |> ignore;
      },
    )
  );
