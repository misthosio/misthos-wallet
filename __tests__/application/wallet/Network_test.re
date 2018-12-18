open Jest;

open Expect;

let () =
  describe("Income addresses", () => {
    /* test("mainnetIncomeAddress can be used", () => */
    /*   expect( */
    /*     Bitcoin.( */
    /*       Network.incomeAddress(Mainnet) */
    /*       |. Address.toOutputScript(Network.bitcoinNetwork(Mainnet)) */
    /*       |. Address.fromOutputScript(Network.bitcoinNetwork(Mainnet)) */
    /*     ), */
    /*   ) */
    /*   |> toEqual(Network.incomeAddress(Mainnet)) */
    /* ); */
    test("testnetIncomeAddress can be used", () =>
      expect(
        Bitcoin.(
          Network.incomeAddress(Testnet)
          ->(Address.toOutputScript(Network.bitcoinNetwork(Testnet)))
          ->(Address.fromOutputScript(Network.bitcoinNetwork(Testnet)))
        ),
      )
      |> toEqual(Network.incomeAddress(Testnet))
    );
    test("regtestIncomeAddress can be used", () =>
      expect(
        Bitcoin.(
          Network.incomeAddress(Regtest)
          ->(Address.toOutputScript(Network.bitcoinNetwork(Regtest)))
          ->(Address.fromOutputScript(Network.bitcoinNetwork(Regtest)))
        ),
      )
      |> toEqual(Network.incomeAddress(Regtest))
    );
  });
