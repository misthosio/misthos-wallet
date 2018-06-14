Helpers.enableHttpRequests();

open Belt;

open Jest;
open Expect;

open Bitcoin;

open WalletTypes;

let script = Ops.([|op_DEPTH, op_1, op_EQUAL, op_NIP|]);
/* let script = Ops.([|op_1, op_EQUAL|]); */
let encodeOutput = () => Script.compile(script);

let () =
  describe("thing", () => {
    let compiledScript = encodeOutput();
    let redeemScript =
      Script.WitnessScriptHash.Output.encode(
        Crypto.sha256FromBuffer(compiledScript),
      );

    let outputScript =
      Script.ScriptHash.Output.encode(Crypto.hash160(redeemScript));
    let displayAddress =
      Address.fromOutputScript(outputScript, Networks.testnet);
    let utxos = ref([]);
    beforeAllPromise(~timeout=40000, () =>
      Js.Promise.(
        Helpers.faucet([(displayAddress, BTC.fromSatoshis(10000L))])
        |> then_(us => {
             utxos := us;
             resolve();
           })
      )
    );

    testPromise("hello", () => {
      let txB = TxBuilder.createWithNetwork(Networks.testnet);
      txB |> TxBuilder.addOutput(Helpers.faucetAddress, 9000.) |> ignore;
      let inX = txB |> TxBuilder.addInput(List.headExn(utxos^).txId, 0);
      let tx = txB |> TxBuilder.buildIncomplete;
      tx |. Transaction.setInputScript(inX, Script.compile([|redeemScript|]));
      tx
      |. Transaction.setWitness(
           inX,
           [|Node.Buffer.fromString(""), compiledScript|],
         );
      Js.Promise.(
        Helpers.broadcastTransaction(tx)
        |> then_(result => {
             Js.log(result);
             expect(result) |> toEqual("") |> resolve;
           })
      );
    });
  });
