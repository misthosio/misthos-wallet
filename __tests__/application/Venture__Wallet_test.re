open Jest;

open WalletHelpers;

let () =
  describe("nextIncomeAddress", () => {
    let (userA, userB, userC) = Fixtures.threeUserSessions;
    let log =
      L.(
        Fixtures.createVenture(userA)
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
    log
    |> constructState
    |> testNextIncomeAddress(userA, "2MtvnaAWnfkSBU7NbpSfMRrCKSTgLeRVd93")
    |> testNextIncomeAddress(userB, "2NGFyACMJG6KJ9Db92n9MM2nNtR4r4tUNFW")
    |> testNextIncomeAddress(userA, "2NCAxqkK9bhRQWssyrMqJhXS65xmUCWraXT")
    |> ignore;
    let log =
      L.(
        log
        |> withPartner(userC, ~supporters=[userA, userB])
        |> withCustodian(userC, ~supporters=[userA, userB, userC])
        |> withCustodianKeyChain(userC)
        |> withAccountKeyChainIdentified
        |> withAccountKeyChainActivated(userA)
        |> withAccountKeyChainActivated(userB)
        |> withAccountKeyChainActivated(userC)
      );
    log
    |> constructState
    |> testNextIncomeAddress(userC, "2NCGfPo6ehd2cgwFNE2ocqUFpv8rtcN3TGj")
    |> testNextIncomeAddress(userB, "2N1sd2funBMd3ntLSbJrALAz3CJxEVsAPV7")
    |> testNextIncomeAddress(userC, "2N85sud6RgkaAEPitqdrNXsMbADYzXCWc7T")
    |> ignore;
  });
