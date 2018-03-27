Helpers.enableHttpRequests();

open Jest;

open Expect;

open Bitcoin;

let () = {
  let config: BitcoindClient.config = {
    bitcoindUrl: "http://localhost:18322",
    rpcUser: "bitcoin",
    rpcPassword: "bitcoin"
  };
  let keyA = ECPair.makeRandomWithNetwork(Networks.testnet);
  let keyB = ECPair.makeRandomWithNetwork(Networks.testnet);
  let tenSats = BTC.fromSatoshis(10L);
  describe("faucet", () =>
    testPromise("Can fund an address", () =>
      Js.Promise.(
        Helpers.faucet([
          (keyA |> ECPair.getAddress, tenSats),
          (keyB |> ECPair.getAddress, tenSats)
        ])
        |> then_((_) =>
             BitcoindClient.getUTXOs(
               config,
               [keyA |> ECPair.getAddress, keyB |> ECPair.getAddress]
             )
           )
        |> then_((utxos: list(BitcoindClient.bitcoindUTXO)) =>
             resolve(
               expect((List.hd(utxos).amount, List.nth(utxos, 1).amount))
               |> toEqual((tenSats, tenSats))
             )
           )
      )
    )
  );
};
