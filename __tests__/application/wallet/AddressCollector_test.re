open Jest;

open Expect;

module G = Generators;

module L = G.Log;

module F = Fixtures;

open WalletTypes;

open Belt;

let constructState = log =>
  log
  |> L.reduce(
       (state, {event}: EventLog.item) =>
         state |> AddressCollector.apply(event),
       AddressCollector.make(),
     );

let () =
  describe("AddressCollector", () =>
    F.withCached(
      ~scope="AddressCollector",
      "collects addresses",
      () => F.threeUserSessionsArray,
      sessions => {
        let (user1, _user2) = G.twoUserSessionsFromArray(sessions);
        L.(
          createVenture(user1)
          |> withFirstPartner(user1)
          |> withAccount(~supporter=user1)
          |> withCustodian(user1, ~supporters=[user1])
          |> withCustodianKeyChain(user1)
          |> withAccountKeyChainIdentified
          |> withAccountKeyChainActivated(user1)
          |> withIncomeAddressExposed(user1)
        );
      },
      (_sessions, log) =>
        test("has the address", () =>
          expect(
            constructState(log).exposedAddresses
            |. Map.String.getExn("2N58QKSCL94bLqcYFfFJrq7HdtVC34snTfU"),
          )
          |> toEqual(
               {
                 nCoSigners: 1,
                 nPubKeys: 1,
                 coordinates: (
                   0 |> AccountIndex.fromInt,
                   "fe8c5350b6440fbe11aca1ed14078f081a878b8f5583168bbb89ac18d75f97ea",
                   0 |> CoSignerIndex.fromInt,
                   ChainIndex.externalChain,
                   0 |> AddressIndex.fromInt,
                 ),
                 witnessScript: "512103599363809c8aa6b8e472457a56e367de5d91030520ff4dc5fa01f034833df59b51ae",
                 redeemScript: "00200ae0ad43c6b4696d0651651354867868e33905109ad0186afdb1629c166fdf4b",
                 address: "2N58QKSCL94bLqcYFfFJrq7HdtVC34snTfU",
               }: Address.t,
             )
        ),
    )
  );
