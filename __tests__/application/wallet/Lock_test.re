open Jest;
open Expect;

open Bitcoin;

let encodeOutput = () => Script.compile(Ops.([|op_DEPTH, op_1, op_EQUAL|]));

let () =
  describe("thing", () =>
    test("hello", () => {
      open Script;
      let compiledScript = encodeOutput();
      let redeemScript = compiledScript;
      /* WitnessScriptHash.Output.encode( */
      /* Crypto.sha256FromBuffer(compiledScript); */
      /* ); */

      let outputScript =
        ScriptHash.Output.encode(Crypto.hash160(redeemScript));
      let displayAddress =
        Address.fromOutputScript(outputScript, Networks.testnet);

      expect((compiledScript |> Utils.bufToHex, displayAddress))
      |> toEqual(("", ""));
    })
  );
