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
        |> then_((utxos: list(BitcoindClient.bitcoindUTXO)) =>
             resolve(expect(List.hd(utxos).satoshis) |> toBe(1001010000000.))
           )
      )
    )
  );
  describe("broadcastTransaction", () =>
    Bitcoin.(
      testPromise("Returns something", () => {
        let destination = ECPair.makeRandomWithNetwork(Networks.testnet);
        let key =
          ECPair.fromWIFWithNetwork(
            "92Qba5hnyWSn5Ffcka56yMQauaWY6ZLd91Vzxbi4a9CCetaHtYj",
            Networks.testnet
          );
        Js.Promise.(
          BitcoindClient.getUTXOs(config, key |> ECPair.getAddress)
          |> then_((utxos: list(BitcoindClient.bitcoindUTXO)) => {
               let utxo = utxos |> List.hd;
               let txBuilder =
                 TxBuilder.createWithOptions(~network=Networks.testnet, ());
               TxBuilder.addInput(txBuilder, utxo.txId, utxo.txOutputN)
               |> ignore;
               TxBuilder.addOutput(
                 txBuilder,
                 ECPair.getAddress(destination),
                 10. *. BitcoindClient.satoshisPerBTC
               )
               |> ignore;
               TxBuilder.addOutput(
                 txBuilder,
                 ECPair.getAddress(key),
                 10000. *. BitcoindClient.satoshisPerBTC
               )
               |> ignore;
               TxBuilder.sign(txBuilder, 0, key);
               BitcoindClient.broadcastTransaction(
                 config,
                 txBuilder |> TxBuilder.build
               );
             })
          |> then_(result => {
               Js.log(result);
               Js.log(
                 Node.Child_process.execSync(
                   "bitcoin-cli -regtest -rpcuser=bitcoin -rpcpassword=bitcoin -rpcport=18322 generate 2",
                   Node.Child_process.option(
                     ~cwd=dirname,
                     ~encoding="utf8",
                     ()
                   )
                 )
               );
               BitcoindClient.getUTXOs(config, ECPair.getAddress(destination));
             })
          |> then_((utxos: list(BitcoindClient.bitcoindUTXO)) =>
               resolve(
                 expect(List.hd(utxos).satoshis)
                 |> toBe(10. *. BitcoindClient.satoshisPerBTC)
               )
             )
        );
      })
    )
  );
};
