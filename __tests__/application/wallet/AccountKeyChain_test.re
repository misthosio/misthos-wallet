open Jest;

open Expect;

open WalletTypes;

module G = Generators;

let () =
  describe("Collection", () => {
    let (user1, user2) = G.twoUserSessions();
    let accountKeyChain1 = G.accountKeyChain([user1, user2]);
    let accountKeyChain2 = G.accountKeyChain(~keyChainIdx=1, [user1, user2]);
    let keyChains =
      []
      |> AccountKeyChain.Collection.add(accountKeyChain2)
      |> AccountKeyChain.Collection.add(accountKeyChain1);
    test("lookup", () =>
      expect(
        keyChains
        |> AccountKeyChain.Collection.lookup(
             AccountIndex.default,
             AccountKeyChainIndex.first,
           ),
      )
      |> toEqual(accountKeyChain1)
    );
    test("latest", () =>
      expect(
        keyChains |> AccountKeyChain.Collection.latest(AccountIndex.default),
      )
      |> toEqual(accountKeyChain2)
    );
  });
