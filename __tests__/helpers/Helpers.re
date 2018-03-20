open Bitcoin;

type utxo = BitcoindClient.bitcoindUTXO;

let enableHttpRequests = () => [%bs.raw
  {| global.XMLHttpRequest = require('xmlhttprequest').XMLHttpRequest |}
];

let faucetKey =
  ECPair.fromWIFWithNetwork(
    "92Qba5hnyWSn5Ffcka56yMQauaWY6ZLd91Vzxbi4a9CCetaHtYj",
    Networks.testnet
  );

let bitcoindConfig: BitcoindClient.config = {
  bitcoindUrl: "http://localhost:18322",
  rpcUser: "bitcoin",
  rpcPassword: "bitcoin"
};

let defaultFee = BTC.fromSatoshis(600L);

let selectUTXOs = (utxos, totalAmount) => {
  let utxos =
    utxos
    |> List.filter(({confirmations}: utxo) => confirmations > 0)
    |> List.sort((u1: utxo, u2: utxo) =>
         u1.amount |> BTC.comparedTo(u2.amount)
       );
  utxos
  |> List.fold_left(
       ((result, total), utxo: utxo) =>
         if (total |> BTC.gt(totalAmount |> BTC.plus(defaultFee))) {
           (result, total);
         } else {
           ([utxo, ...result], total |> BTC.plus(utxo.amount));
         },
       ([], BTC.zero)
     );
};

exception FaucetEmpty;

let getUTXOs = address => BitcoindClient.getUTXOs(bitcoindConfig, address);

let broadcastTransaction = tx =>
  BitcoindClient.broadcastTransaction(bitcoindConfig, tx)
  |> Js.Promise.then_(result => {
       Node.Child_process.execSync(
         "bitcoin-cli -regtest -rpcuser=bitcoin -rpcpassword=bitcoin -rpcport=18322 generate 2",
         Node.Child_process.option(~encoding="utf8", ())
       )
       |> ignore;
       Js.Promise.resolve(result);
     });

let fundAddress = (address, values, utxos) => {
  let totalValues =
    values |> List.fold_left((n, v) => n |> BTC.plus(v), BTC.zero);
  let (inputs, totalIn) = selectUTXOs(utxos, totalValues);
  if (totalIn < totalValues) {
    raise(FaucetEmpty);
  };
  let txB = TxBuilder.createWithNetwork(Networks.testnet);
  inputs
  |> List.iter((utxo: utxo) =>
       txB |> TxBuilder.addInput(utxo.txId, utxo.txOutputN) |> ignore
     );
  values
  |> List.iter(v =>
       txB |> TxBuilder.addOutput(address, v |> BTC.toSatoshisFloat) |> ignore
     );
  let remainder = totalIn |> BTC.minus(totalValues) |> BTC.minus(defaultFee);
  txB
  |> TxBuilder.addOutput(
       faucetKey |> ECPair.getAddress,
       remainder |> BTC.toSatoshisFloat
     )
  |> ignore;
  inputs |> List.iteri((i, _utxo) => txB |> TxBuilder.sign(i, faucetKey));
  Js.Promise.(
    broadcastTransaction(txB |> TxBuilder.build)
    |> then_((_) => BitcoindClient.getUTXOs(bitcoindConfig, address))
  );
};

let faucet = (address, values) =>
  Js.Promise.(
    BitcoindClient.getUTXOs(bitcoindConfig, faucetKey |> ECPair.getAddress)
    |> then_(fundAddress(address, values))
  );

let rec rangeTo = (x, y) =>
  if (x == y) {
    [];
  } else {
    [x, ...rangeTo(x + 1, y)];
  };

let displayTx = txHex => {
  let versionLength = 4 * 2;
  let output = "Version:       " ++ String.sub(txHex, 0, versionLength);
  let segwitMarkerPos = versionLength;
  let segwitMarkerLength = 1 * 2;
  let output =
    output
    ++ "\nSegwit Marker: "
    ++ String.sub(txHex, segwitMarkerPos, segwitMarkerLength);
  let segwitFlagPos = segwitMarkerPos + segwitMarkerLength;
  let segwitFlagLength = 1 * 2;
  let output =
    output
    ++ "\nSegwit Marker: "
    ++ String.sub(txHex, segwitFlagPos, segwitFlagLength);
  let nInputsPos = segwitFlagPos + segwitFlagLength;
  let nInputsLength = 1 * 2;
  let output =
    output
    ++ "\nNum Inputs:    "
    ++ String.sub(txHex, nInputsPos, nInputsLength);
  let nInputs =
    int_of_string("0x" ++ String.sub(txHex, nInputsPos, nInputsLength));
  let (pos, loopOut) =
    rangeTo(0, nInputs)
    |> List.fold_left(
         ((pos, output), _n) => {
           let prevOutputHashPos = pos;
           let prevOutputHashLength = 32 * 2;
           let output =
             output
             ++ "\nPrevOutHash:   "
             ++ String.sub(txHex, prevOutputHashPos, prevOutputHashLength);
           let prevOutputIndexPos = prevOutputHashPos + prevOutputHashLength;
           let prevOutputIndexLength = 4 * 2;
           let output =
             output
             ++ "\nPrevOutIndex:  "
             ++ String.sub(txHex, prevOutputIndexPos, prevOutputIndexLength);
           let scriptLengthPos = prevOutputIndexPos + prevOutputIndexLength;
           let scriptLengthLength = 1 * 2;
           let output =
             output
             ++ "\nScriptLength:  "
             ++ String.sub(txHex, scriptLengthPos, scriptLengthLength);
           let scriptPos = scriptLengthPos + scriptLengthLength;
           let scriptLength =
             int_of_string(
               "0x" ++ String.sub(txHex, scriptLengthPos, scriptLengthLength)
             )
             * 2;
           let output =
             output
             ++ "\nScript ("
             ++ string_of_int(scriptLength / 2)
             ++ "):   "
             ++ String.sub(txHex, scriptPos, scriptLength);
           let sequencePos = scriptPos + scriptLength;
           let sequenceLength = 4 * 2;
           let output =
             output
             ++ "\nScriptLength:  "
             ++ String.sub(txHex, sequencePos, sequenceLength);
           (sequencePos + sequenceLength, output);
         },
         (nInputsPos + nInputsLength, "")
       );
  let output = output ++ loopOut;
  let outputCountPos = pos;
  let outputCountLength = 2 * 1;
  let output =
    output
    ++ "\nOutputCount:   "
    ++ String.sub(txHex, outputCountPos, outputCountLength);
  let outputCount =
    int_of_string(
      "0x" ++ String.sub(txHex, outputCountPos, outputCountLength)
    );
  let (pos, loopOut) =
    rangeTo(0, outputCount)
    |> List.fold_left(
         ((pos, output), _n) => {
           let outputValPos = pos;
           let outputValLength = 8 * 2;
           let output =
             output
             ++ "\nOutputVal:     "
             ++ String.sub(txHex, outputValPos, outputValLength);
           let scriptLengthPos = outputValPos + outputValLength;
           let scriptLengthLength = 1 * 2;
           let output =
             output
             ++ "\nScriptLength:  "
             ++ String.sub(txHex, scriptLengthPos, scriptLengthLength);
           let scriptPos = scriptLengthPos + scriptLengthLength;
           let scriptLength =
             int_of_string(
               "0x" ++ String.sub(txHex, scriptLengthPos, scriptLengthLength)
             )
             * 2;
           let output =
             output
             ++ "\nScript ("
             ++ string_of_int(scriptLength / 2)
             ++ "):   "
             ++ String.sub(txHex, scriptPos, scriptLength);
           (scriptPos + scriptLength, output);
         },
         (outputCountPos + outputCountLength, "")
       );
  let output = output ++ loopOut;
  let (pos, loopOut) =
    rangeTo(0, nInputs)
    |> List.fold_left(
         ((pos, output), _n) => {
           let numberOfStackItemsPos = pos;
           let numberOfStackItemsLength = 1 * 2;
           let output =
             output
             ++ "\nNumStackItems:  "
             ++ String.sub(
                  txHex,
                  numberOfStackItemsPos,
                  numberOfStackItemsLength
                );
           let numberOfStackItems =
             int_of_string(
               "0x"
               ++ String.sub(
                    txHex,
                    numberOfStackItemsPos,
                    numberOfStackItemsLength
                  )
             );
           let (pos, loopOut) =
             rangeTo(0, numberOfStackItems)
             |> List.fold_left(
                  ((pos, output), _n) => {
                    let stackSizeOfItemPos = pos;
                    let stackSizeOfItemLength = 1 * 2;
                    let output =
                      output
                      ++ "\nStackSizeOfItem:  "
                      ++ String.sub(
                           txHex,
                           stackSizeOfItemPos,
                           stackSizeOfItemLength
                         );
                    let stackItemPos =
                      stackSizeOfItemPos + stackSizeOfItemLength;
                    let stackItemLength =
                      int_of_string(
                        "0x"
                        ++ String.sub(
                             txHex,
                             stackSizeOfItemPos,
                             stackSizeOfItemLength
                           )
                      )
                      * 2;
                    let output =
                      output
                      ++ "\nStackItem ("
                      ++ string_of_int(stackItemLength / 2)
                      ++ "):   "
                      ++ String.sub(txHex, stackItemPos, stackItemLength);
                    (stackItemPos + stackItemLength, output);
                  },
                  (numberOfStackItemsPos + numberOfStackItemsLength, "")
                );
           let output = output ++ loopOut;
           (pos, output);
         },
         (pos, "")
       );
  let output = output ++ loopOut;
  let lockTimePos = pos;
  let lockTimeLength = 4 * 2;
  let output =
    output
    ++ "\nLocktime:     "
    ++ String.sub(txHex, lockTimePos, lockTimeLength);
  Js.log(output);
  Js.log(pos + lockTimeLength);
  Js.log(String.length(txHex));
};
