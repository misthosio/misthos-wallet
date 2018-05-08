open Jest;

open WalletHelpers;

let () =
  describe("nextIncomeAddress", () => {
    let (userA, userB, _) = Fixtures.threeUserSessions;
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
  });
