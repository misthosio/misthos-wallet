open Bitcoin;

let minimumSatoshisInUTXO = 10000.;

type t = {
  witnessScript: string,
  redeemScript: string,
  address: string
};

type payoutTx = {
  inputValues: list((int, float)),
  txHex: string
};

type payoutSignature = {signatures: list((int, int, string))};

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
              utxo.satoshis >= minimumSatoshisInUTXO
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
              (n, utxo: BitcoindClient.bitcoindUTXO) => n +. utxo.satoshis,
              0.
            )
         |> resolve
       )
  );

let signPayoutTx =
    (
      {inputValues, txHex},
      key,
      {redeemScript, witnessScript},
      ~network=Networks.bitcoin
    ) => {
  let txB = TxBuilder.fromTransactionWithNetwork(Tx.fromHex(txHex), network);
  inputValues
  |> List.iter(((index, witnessValue)) =>
       txB
       |> TxBuilder.signSegwit(
            index,
            key,
            ~redeemScript=Utils.bufFromHex(redeemScript),
            ~witnessValue,
            ~witnessScript=Utils.bufFromHex(witnessScript)
          )
     );
  {inputValues, txHex: txB |> TxBuilder.buildIncomplete |> Tx.toHex};
};

let preparePayoutTx =
    ({address} as wallet, destinations, systemKey, ~network=Networks.bitcoin) => {
  let txB = TxBuilder.createWithOptions(~network, ());
  destinations
  |> List.iter(((address, satoshis)) =>
       txB |> TxBuilder.addOutput(address, satoshis) |> ignore
     );
  Js.Promise.(
    getUTXOs(address)
    |> then_(utxos =>
         {
           inputValues:
             utxos
             |> List.map(
                  ({txId, txOutputN, satoshis}: BitcoindClient.bitcoindUTXO) =>
                  (txB |> TxBuilder.addInput(txId, txOutputN), satoshis)
                ),
           txHex: txB |> TxBuilder.buildIncomplete |> Tx.toHex
         }
         |> resolve
       )
    |> then_(payoutTx =>
         signPayoutTx(payoutTx, systemKey, wallet, ~network) |> resolve
       )
  );
};
