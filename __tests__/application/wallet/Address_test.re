open Jest;

open Expect;

module G = Generators;

module E = G.Event;

open PrimitiveTypes;

open WalletTypes;

let testCoordinates =
    (
      expected,
      (accountIdx, accountKeyChainIdx, coSignerIdx, chainIdx, addressIdx),
    ) =>
  test("should match", () =>
    (
      accountIdx |> AccountIndex.toInt,
      accountKeyChainIdx |> AccountKeyChainIndex.toInt,
      coSignerIdx |> CoSignerIndex.toInt,
      chainIdx |> ChainIndex.toInt,
      addressIdx |> AddressIndex.toInt,
    )
    |> expect
    |> toEqual(expected)
  );

let findCoSignerIndex =
    (
      {userId: coSigner}: Session.Data.t,
      {custodianKeyChains}: AccountKeyChain.t,
    ) =>
  custodianKeyChains
  |> List.map(chain =>
       (chain |> fst, chain |> snd |> CustodianKeyChain.hdNode)
     )
  |> List.sort(((_, chainA), (_, chainB)) =>
       compare(
         chainA |> Bitcoin.HDNode.getPublicKeyBuffer |> Utils.bufToHex,
         chainB |> Bitcoin.HDNode.getPublicKeyBuffer |> Utils.bufToHex,
       )
     )
  |> List.mapi((i, (user, _)) =>
       UserId.eq(user, coSigner) ? Some(i) : None
     )
  |> List.find(Js.Option.isSome)
  |> Js.Option.getExn;

let () =
  describe("Coordinates", () => {
    let (user1, user2) = G.twoUserSessions();
    let accountKeyChain = G.accountKeyChain([user1, user2]);
    let coSignerIdx = findCoSignerIndex(user1, accountKeyChain);
    describe("first coordinates", () =>
      NewAddress.Coordinates.nextInternal(user1.userId, [], accountKeyChain)
      |> testCoordinates((0, 0, coSignerIdx, 1, 0))
    );
    describe("next coordinates", () => {
      let coordinates1 =
        NewAddress.Coordinates.nextExternal(
          user1.userId,
          [],
          accountKeyChain,
        );
      let coordinates2 =
        NewAddress.Coordinates.nextExternal(
          user1.userId,
          [coordinates1],
          accountKeyChain,
        );
      NewAddress.Coordinates.nextExternal(
        user1.userId,
        [coordinates2, coordinates1],
        accountKeyChain,
      )
      |> testCoordinates((0, 0, coSignerIdx, 0, 2));
    });
  });
