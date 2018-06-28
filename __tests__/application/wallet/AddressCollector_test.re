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
          F.createVenture(user1)
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
            |. Map.String.getExn("2N7bomZmWhymdGttGbZZRYZ5QBS2TLnoMMJ"),
          )
          |> toEqual(
               {
                 nCoSigners: 1,
                 nPubKeys: 1,
                 coordinates: (
                   0 |> AccountIndex.fromInt,
                   "8974ad69910afdca42d4c7c08c094c8d2a9d454d0f02b5b101eb7abd30a06d30",
                   0 |> CoSignerIndex.fromInt,
                   ChainIndex.externalChain,
                   0 |> AddressIndex.fromInt,
                 ),
                 witnessScript: "51210309995c255526c59ac6a6563832a838fbb9917305d84d3c11393e575238b4e9aa51ae",
                 redeemScript: "0020f9a067ba831974aef9e6e8363e437d2660ca5120ab23c0c1acf32aa1605894c8",
                 displayAddress: "2N7bomZmWhymdGttGbZZRYZ5QBS2TLnoMMJ",
                 sequence: None,
               }: Address.t,
             )
        ),
    )
  );
