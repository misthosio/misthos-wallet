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
        |> withAccountKeyChain([userA, userB])
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
        |> withCustodian(userC, ~supporters=[userA, userB])
        |> withAccountKeyChain(~keyChainIdx=1, [userA, userB, userC])
      );
    log
    |> constructState
    |> testNextIncomeAddress(userC, "2Mu1gDoDnhGFJqYxAcRan17HyU9oLwty35g")
    |> testNextIncomeAddress(userB, "2N5aKGngKwUDhUYdiT6QQCbSEC4aU2GpAJE")
    |> testNextIncomeAddress(userC, "2NBh9WTFfbiTSqQQpy8NHuU8gZv362vMsFf")
    |> ignore;
  });
