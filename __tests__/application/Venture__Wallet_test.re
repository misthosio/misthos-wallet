open Jest;

open WalletHelpers;

let () =
  describe("Venture__Wallet", () =>
    describe("nextIncomeAddress", () => {
      let (user1, user2, user3) = F.threeUserSessions;
      let log =
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
      log
      |> constructState
      |> testNextIncomeAddress(user1, "2MsrfLGP6dhR1RgaW5y6ov6gww3t6fMBadm")
      |> testNextIncomeAddress(user2, "2MsHBjkYY14C5bBtqE6YsQtrcZZ8cuzAKc6")
      |> testNextIncomeAddress(user1, "2N6YtJ28KAB5MakPtXmdhAzenvhD5tuY5zz")
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
      |> testNextIncomeAddress(user3, "2MtBbL8QZx3vKj8E51yjNxZ2VM7hTqT8M9F")
      |> testNextIncomeAddress(user2, "2N5NLNeBc52TSyKnUxGZb2V1QsjaeFdfYcD")
      |> testNextIncomeAddress(user3, "2NBgMc8ud9vUxsLuDLmgtwSju7wFs3uFqiN")
      |> ignore;
    })
  );
