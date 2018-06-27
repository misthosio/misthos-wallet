open Belt;

module B = Bitcoin;

type input = {signatures: array(Node.buffer)};
type t = {
  tx: B.Transaction.t,
  inputs: array(input),
};

let extractInputs = tx =>
  tx##ins
  |. Array.map(input => {
       let witness = input##witness;
       let witness =
         witness |> Array.slice(~offset=1, ~len=(witness |> Array.length) - 2);
       {signatures: witness};
     });

let make = hex => {
  let tx = B.Transaction.fromHex(hex);
  {tx, inputs: extractInputs(tx)};
};

let needsSigning = (idx, {inputs}) => {
  let input = inputs |. Array.getExn(idx);
  switch (input.signatures) {
  | [||] => true
  | sigs =>
    sigs
    |. Array.someU((. sig_) => sig_ |> B.Script.isCanonicalSignature == false)
  };
};

let sign =
    (
      idx,
      keyPair,
      nCoSigners,
      ~redeemScript,
      ~witnessValue,
      ~witnessScript,
      {tx, inputs},
    ) => {
  let witnessBuf = witnessScript |> Utils.bufFromHex;
  tx |. B.Transaction.setInputScript(idx, redeemScript |> Utils.bufFromHex);
  let signatureHash =
    tx
    |. B.Transaction.hashForWitnessV0(
         idx,
         witnessBuf,
         witnessValue |> BTC.toSatoshisFloat,
         B.Transaction.sighashAll,
       );
  let signature =
    keyPair
    |> B.ECPair.sign(signatureHash)
    |. B.ECSignature.toScriptSignature(B.Transaction.sighashAll);
  let input = inputs |. Array.getExn(idx);
  let signatures =
    switch (input.signatures) {
    | [||] =>
      Array.concat(
        Array.makeByU(nCoSigners - 1, (. _) => BufferExt.makeWithSize(0)),
        [|signature|],
      )
    | sigs =>
      let (insert, _) =
        sigs
        |. Array.reduceU(((-1), 0), (. (res, idx), sig_) =>
             if (sig_ |> BufferExt.length == 0) {
               (idx, idx + 1);
             } else {
               (res, idx + 1);
             }
           );
      sigs
      |. Array.mapWithIndexU((. idx, sig_) =>
           idx == insert ? signature : sig_
         );
    };
  tx
  |. B.Transaction.setWitness(
       idx,
       Array.concatMany([|
         [|BufferExt.makeWithSize(0)|],
         signatures,
         [|witnessBuf|],
       |]),
     );
  {tx, inputs: extractInputs(tx)};
};
