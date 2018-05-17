open Jest;

open Expect;

open PrimitiveTypes;

open WalletTypes;

module G = Generators;

let () = {
  describe("Identifier", () =>
    test("Creates uniq hash of the custodianKeyChains", () => {
      let (user1, user2, _user3) = Fixtures.threeUserSessions;
      let custodianKeyChain1 =
        G.custodianKeyChain(
          ~ventureId=VentureId.fromString("test"),
          ~keyChainIdx=0,
          user1,
        );
      let custodianKeyChain2 =
        G.custodianKeyChain(
          ~ventureId=VentureId.fromString("test"),
          ~keyChainIdx=0,
          user2,
        );
      let identifier =
        AccountKeyChain.Identifier.make(
          1,
          [
            (user1.userId, custodianKeyChain1),
            (user2.userId, custodianKeyChain2),
          ],
        );
      expect(identifier)
      |> toEqual(
           "cba2209dcdabcc79ed56276593cc1811280afdbe9d06a19a018e9dcf431d647a",
         );
    })
  );
  describe("Collection", () => {
    let (user1, user2) = G.twoUserSessions();
    let accountKeyChain1 = G.accountKeyChain([user1, user2]);
    let accountKeyChain2 = G.accountKeyChain(~keyChainIdx=1, [user1, user2]);
    let keyChains =
      AccountKeyChain.Collection.empty
      |> AccountKeyChain.Collection.add(accountKeyChain2)
      |> AccountKeyChain.Collection.add(accountKeyChain1);
    test("lookup", () =>
      expect(
        keyChains
        |> AccountKeyChain.Collection.lookup(
             AccountIndex.default,
             accountKeyChain1.identifier,
           ),
      )
      |> toEqual(accountKeyChain1)
    );
  });
};
