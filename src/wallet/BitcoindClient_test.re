[%bs.raw
  {| global.XMLHttpRequest = require('xmlhttprequest').XMLHttpRequest |}
];

open Jest;

open Expect;

let () = {
  beforeAll(() => TestHelpers.startBitcoind());
  afterAll(() => TestHelpers.stopBitcoind());
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
             resolve(expect(List.hd(utxos).satoshis) |> toBe(101010000000.))
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
                 1000. *. BitcoindClient.satoshisPerBTC
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
                   Node.Child_process.option(~encoding="utf8", ())
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
