[%bs.raw
  {| global.XMLHttpRequest = require('xmlhttprequest').XMLHttpRequest |}
];

open Jest;

open Expect;

let () = {
  let dirname =
    switch [%node __dirname] {
    | Some(name) => name
    | None => raise(Not_found)
    };
  beforeAll(() => {
    Js.log("Starting bitcoind");
    Js.log(
      Node.Child_process.execSync(
        dirname ++ "/test_helpers/start_bitcoind.sh",
        Node.Child_process.option(~cwd=dirname, ~encoding="utf8", ())
      )
    );
  });
  afterAll(() => {
    Js.log("Stopping bitcoind");
    Js.log(
      Node.Child_process.execSync(
        dirname ++ "/test_helpers/stop_bitcoind.sh",
        Node.Child_process.option(~cwd=dirname, ~encoding="utf8", ())
      )
    );
  });
  let config: BitcoindClient.config = {
    bitcoindUrl: "http://localhost:18322",
    rpcUser: "bitcoin",
    rpcPassword: "bitcoin"
  };
  describe("GetBlockHeight", () =>
    testPromise("is at 700", () =>
      Js.Promise.(
        BitcoindClient.getBlockHeight(config)
        |> then_(blockHeight => resolve(expect(blockHeight) |> toBe(700)))
      )
    )
  );
  describe("getUTXOs", () =>
    testPromise("Returns UTXOs", () =>
      Js.Promise.(
        BitcoindClient.getUTXOs(config, "mgWUuj1J1N882jmqFxtDepEC73Rr22E9GU")
        |> then_((utxos: list(bitcoindUTXO) => {
             Js.log(utxos);
             resolve(expect(List.hd(utxos).satoshis) |> toBe(1000010000000.));
           })
      )
    )
  );
};
