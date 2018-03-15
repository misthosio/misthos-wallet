[%bs.raw
  {| global.XMLHttpRequest = require('xmlhttprequest').XMLHttpRequest |}
];

open Jest;

open Expect;

open Bitcoin;

let () = {
  let config: BitcoindClient.config = {
    bitcoindUrl: "http://localhost:18322",
    rpcUser: "bitcoin",
    rpcPassword: "bitcoin"
  };
  let key = ECPair.makeRandomWithNetwork(Networks.testnet);
  describe("faucet", () =>
    testPromise("Can fund an address", () =>
      Js.Promise.(
        Helpers.faucet(key |> ECPair.getAddress, [10., 10.])
        |> then_((_) =>
             BitcoindClient.getUTXOs(config, key |> ECPair.getAddress)
           )
        |> then_((utxos: list(BitcoindClient.bitcoindUTXO)) =>
             resolve(
               expect((List.hd(utxos).satoshis, List.nth(utxos, 1).satoshis))
               |> toEqual((10., 10.))
             )
           )
      )
    )
  );
};
