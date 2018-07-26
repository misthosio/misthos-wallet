Helpers.enableHttpRequests();

open Jest;

open Expect;

open Bitcoin;

open WalletTypes;

let () = {
  let config: BitcoindClient.config = {
    bitcoindUrl: "http://localhost:18322",
    rpcUser: "bitcoin",
    rpcPassword: "bitcoin",
  };
  let keyA = ECPair.makeRandomWithNetwork(Networks.testnet);
  let keyB = ECPair.makeRandomWithNetwork(Networks.testnet);
  let tenSats = BTC.fromSatoshis(10L);
  Skip.describe("faucet", () =>
    testPromise("Can fund an address", () =>
      Js.Promise.(
        Helpers.faucet([
          (keyA |> Address.fromKeyPair, tenSats),
          (keyB |> Address.fromKeyPair, tenSats),
        ])
        |> then_(_ =>
             BitcoindClient.getUTXOs(
               config,
               [keyA |> Address.fromKeyPair, keyB |> Address.fromKeyPair],
             )
           )
        |> then_((utxos: list(utxo)) =>
             resolve(
               expect((List.hd(utxos).amount, List.nth(utxos, 1).amount))
               |> toEqual((tenSats, tenSats)),
             )
           )
      )
    )
  );
};
