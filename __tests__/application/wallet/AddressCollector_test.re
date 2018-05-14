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
            |. Map.String.getExn("2MuMHJgoATXXdNCwJk2i6RoFVR78Ma1ngE9"),
          )
          |> toEqual(
               {
                 nCoSigners: 1,
                 nPubKeys: 1,
                 coordinates: (
                   0 |> AccountIndex.fromInt,
                   "594c2d5570c25e865e19a106780e044a2a315ff9c27a2197a19ae3f7cd2bd572",
                   0 |> CoSignerIndex.fromInt,
                   ChainIndex.externalChain,
                   0 |> AddressIndex.fromInt,
                 ),
                 witnessScript: "512102c3f0db0d8765b00e004d92c334bbb39668b26c5514a60e88a03002d7fd5e6dc551ae",
                 redeemScript: "00204b13214aeea3af22812a9dafa3358ab278d3fff5d25d23cfcea859636a9460ff",
                 address: "2MuMHJgoATXXdNCwJk2i6RoFVR78Ma1ngE9",
               }: Address.t,
             )
        ),
    )
  );
