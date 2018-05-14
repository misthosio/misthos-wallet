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
            |. Map.String.getExn("2N7unXRz7bkBsMMCQq3dcxLXXi1sPNq8m2o"),
          )
          |> toEqual(
               {
                 nCoSigners: 1,
                 nPubKeys: 1,
                 coordinates: (
                   0 |> AccountIndex.fromInt,
                   "41f508a17ccd3b6e325be410341fd320d8d72befbb54cddf5723432a340bcc73",
                   0 |> CoSignerIndex.fromInt,
                   ChainIndex.externalChain,
                   0 |> AddressIndex.fromInt,
                 ),
                 witnessScript: "5121032a66ac40d30d81a2d0dca008ae11f0fed2a00896fe8e61350b2f9d0ca6256b6351ae",
                 redeemScript: "0020a598432cbaab0e4039f2e240a97097e4f9a1e1763edd7ae2ca0021b4268ba8fc",
                 address: "2N7unXRz7bkBsMMCQq3dcxLXXi1sPNq8m2o",
               }: Address.t,
             )
        ),
    )
  );
