open Bitcoin;

let minimumUTXOAmount = BTC.fromSatoshis(10000L);

type t = {
  witnessScript: string,
  redeemScript: string,
  address: string
};

type payoutTx = {
  inputValues: list((int, BTC.t)),
  txHex: string
};

type payoutSignature = list((int, list((int, string))));

let makeAddress = (~network=Networks.bitcoin, m, keys) => {
  open Script;
  let witnessScript =
    Multisig.Output.encode(
      m,
      keys |> List.map(ECPair.getPublicKeyBuffer) |> Array.of_list
    );
  let redeemScript =
    WitnessScriptHash.Output.encode(Crypto.sha256FromBuffer(witnessScript));
  let outputScript = ScriptHash.Output.encode(Crypto.hash160(redeemScript));
  let address = Address.fromOutputScript(outputScript, network);
  {
    witnessScript: Utils.bufToHex(witnessScript),
    redeemScript: Utils.bufToHex(redeemScript),
    address
  };
};

let getUTXOs = address =>
  Js.Promise.(
    BitcoindClient.getUTXOs(
      {
        bitcoindUrl: "http://localhost:18322",
        rpcUser: "bitcoin",
        rpcPassword: "bitcoin"
      },
      address
    )
    |> then_(utxos =>
         utxos
         |> List.filter((utxo: BitcoindClient.bitcoindUTXO) =>
              utxo.amount |> BTC.gte(minimumUTXOAmount)
            )
         |> resolve
       )
  );

let getBalance = ({address}, ~network=Networks.bitcoin) =>
  Js.Promise.(
    getUTXOs(address)
    |> then_(utxos =>
         utxos
         |> List.fold_left(
              (n, utxo: BitcoindClient.bitcoindUTXO) =>
                n |> BTC.plus(utxo.amount),
              BTC.zero
            )
         |> resolve
       )
  );

let preparePayoutTx =
    (
      {address, redeemScript, witnessScript} as wallet,
      destinations,
      reference,
      systemKey,
      ~network=Networks.bitcoin
    ) => {
  let txB = TxBuilder.createWithNetwork(network);
  destinations
  |> List.iter(((address, amount)) =>
       txB
       |> TxBuilder.addOutput(address, amount |> BTC.toSatoshisFloat)
       |> ignore
     );
  txB
  |> TxBuilder.addOutput(
       Script.NullData.Output.encode(reference |> Node_buffer.fromString),
       0.
     );
  Js.Promise.(
    getUTXOs(address)
    |> then_(utxos =>
         {
           inputValues:
             utxos
             |> List.map(
                  ({txId, txOutputN, amount}: BitcoindClient.bitcoindUTXO) =>
                  (txB |> TxBuilder.addInput(txId, txOutputN), amount)
                ),
           txHex: txB |> TxBuilder.buildIncomplete |> Transaction.toHex
         }
         |> resolve
       )
    |> then_(({inputValues}) => {
         inputValues
         |> List.iter(((index, witnessValue)) =>
              txB
              |> TxBuilder.signSegwit(
                   index,
                   systemKey,
                   ~redeemScript=Utils.bufFromHex(redeemScript),
                   ~witnessValue=witnessValue |> BTC.toSatoshisFloat,
                   ~witnessScript=Utils.bufFromHex(witnessScript)
                 )
            );
         {
           inputValues,
           txHex: txB |> TxBuilder.buildIncomplete |> Transaction.toHex
         }
         |> resolve;
       })
  );
};

let getSignatures = txB =>
  txB##inputs
  |> Array.mapi((i, input) =>
       (
         i,
         input##signatures
         |> Array.mapi((i, sigBuf) =>
              (
                i,
                switch (sigBuf |> Js.Nullable.to_opt) {
                | Some(buf) => Some(Utils.bufToHex(buf))
                | None => None
                }
              )
            )
         |> Array.to_list
         |> List.filter(((_, sigString)) => Js.Option.isSome(sigString))
         |> List.map(((i, sigOpt)) => (i, Js.Option.getExn(sigOpt)))
       )
     );

let signPayoutTx =
    (
      {inputValues, txHex},
      key,
      {redeemScript, witnessScript},
      ~network=Networks.bitcoin
    ) => {
  let txB =
    TxBuilder.fromTransactionWithNetwork(Transaction.fromHex(txHex), network);
  let signatures = getSignatures(txB);
  inputValues
  |> List.iter(((index, witnessValue)) =>
       txB
       |> TxBuilder.signSegwit(
            index,
            key,
            ~redeemScript=Utils.bufFromHex(redeemScript),
            ~witnessValue=witnessValue |> BTC.toSatoshisFloat,
            ~witnessScript=Utils.bufFromHex(witnessScript)
          )
     );
  getSignatures(txB)
  |> Array.to_list
  |> List.map(((inputX, sigs)) =>
       (
         inputX,
         sigs
         |> List.filter(((i, _sig)) =>
              ! (signatures[inputX] |> snd |> List.mem_assoc(i))
            )
       )
     );
};

let finalizeTx = ({txHex}, signatures, ~network=Networks.bitcoin) => {
  let txB =
    TxBuilder.fromTransactionWithNetwork(Transaction.fromHex(txHex), network);
  signatures
  |> List.iter(signature =>
       signature
       |> List.iter(((inputIdx, sigs)) =>
            sigs
            |> List.iter(((i, signature)) => {
                 let inputs = txB##inputs;
                 let input = inputs[inputIdx];
                 let signatures = input##signatures;
                 signatures[i] =
                   signature |> Utils.bufFromHex |> Js.Nullable.return;
               })
          )
     );
  TxBuilder.build(txB);
};
