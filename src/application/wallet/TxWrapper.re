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

let pubKeyIndex = (witnessBuf, nCustodians, pubKey) => {
  let chunks = witnessBuf |> B.Script.decompile;
  let pubKeys =
    chunks
    |. Array.slice(
         ~offset=(chunks |> Array.length) - 2 - nCustodians,
         ~len=nCustodians,
       );
  let (idx, _) =
    pubKeys
    |. Array.reduceU(((-1), 0), (. (res, idx), key) =>
         BufferExt.compare(key, pubKey) == 0 ?
           (idx, idx + 1) : (res, idx + 1)
       );
  idx;
};

let sign =
    (
      idx,
      keyPair,
      ~nCustodians,
      ~redeemScript,
      ~witnessValue,
      ~witnessScript,
      {tx, inputs},
    ) => {
  let witnessBuf = witnessScript |> Utils.bufFromHex;
  tx
  |. B.Transaction.setInputScript(
       idx,
       B.Script.compile([|redeemScript |> Utils.bufFromHex|]),
     );
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
  let pubKey = keyPair |> B.ECPair.getPublicKeyBuffer;
  let insert = pubKey |> pubKeyIndex(witnessBuf, nCustodians);
  let input = inputs |. Array.getExn(idx);
  let signatures =
    switch (input.signatures) {
    | [||] => Array.makeByU(nCustodians, (. _) => BufferExt.makeWithSize(0))
    | sigs => sigs
    };
  signatures |. Array.set(insert, signature) |> ignore;
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

let getWitnessBuf = (idx, tx) => {
  let ins = tx##ins;
  let witnessScript = (ins |. Array.getExn(idx))##witness;
  witnessScript |. Array.getExn((witnessScript |> Array.length) - 1);
};

let merge = ({tx, inputs}, {inputs: otherInputs}) => {
  inputs
  |. Array.forEachWithIndexU((. idx, {signatures}) => {
       let otherSigs = (otherInputs |. Array.getExn(idx)).signatures;
       let signatures =
         switch (signatures, otherSigs) {
         | ([||], _) => otherSigs
         | (_, [||]) => signatures
         | _ =>
           Array.reduceReverse2U(
             signatures, otherSigs, [], (. res, sigA, sigB) =>
             [sigA |> B.Script.isCanonicalSignature ? sigA : sigB, ...res]
           )
           |> List.toArray
         };
       let witnessBuf = tx |> getWitnessBuf(idx);
       tx
       |. B.Transaction.setWitness(
            idx,
            Array.concatMany([|
              [|BufferExt.makeWithSize(0)|],
              signatures,
              [|witnessBuf|],
            |]),
          );
     });
  {tx, inputs: extractInputs(tx)};
};

type finalizeResult =
  | Ok(B.Transaction.t)
  | NotEnoughSignatures;
exception NotEnoughSignatures;
let finalize = (usedInputs, {tx, inputs}) =>
  try (
    {
      inputs
      |. Array.forEachWithIndexU((. idx, {signatures}) => {
           let nCoSigners =
             (usedInputs |. Array.getExn(idx): Network.txInput).nCoSigners;
           let signatures =
             signatures
             |. Array.keep(B.Script.isCanonicalSignature)
             |. Array.slice(~offset=0, ~len=nCoSigners);
           if (signatures |> Array.length < nCoSigners) {
             raise(NotEnoughSignatures);
           };
           let witnessBuf = tx |> getWitnessBuf(idx);
           tx
           |. B.Transaction.setWitness(
                idx,
                Array.concatMany([|
                  [|BufferExt.makeWithSize(0)|],
                  signatures,
                  [|witnessBuf|],
                |]),
              );
         });
      Ok(tx);
    }
  ) {
  | NotEnoughSignatures => NotEnoughSignatures
  };
