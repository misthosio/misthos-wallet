/* for reference: https://en.bitcoin.it/wiki/Weight_units */
let version = 4 * 4;

let segwitMarker = 1;

let segwitFlag = 1;

let numOfInputs = 1 * 4;

let previousOutputHash = 32 * 4;

let previousOutputIndex = 4 * 4;

let scriptLength = 1 * 4;

let p2shp2wsh = 35 * 4;

let sequence = 4 * 4;

let outputCount = 1 * 4;

let outputValue = 8 * 4;

let outputScriptSize = 1 * 4;

let numOfStackItems = 1;

let stackSizeOfItem = 1;

let stackSizeOfSignature = 72;

let stackSizeOfMultisigScript = 105;

let lockTime = 4 * 4;

let estimateInputWeight = nCoSigners =>
  previousOutputHash
  + previousOutputIndex
  + scriptLength
  + p2shp2wsh
  + sequence
  + numOfStackItems
  + stackSizeOfItem
  + stackSizeOfMultisigScript
  + (stackSizeOfItem + stackSizeOfSignature)
  * nCoSigners;

let outputWeight = (address, network) =>
  outputValue
  + outputScriptSize
  + (
    Bitcoin.Address.toOutputScript(address, network)
    |> Utils.bufToHex
    |> Utils.hexByteLength
  )
  * 4;

let baseWeight =
  version + segwitMarker + segwitFlag + numOfInputs + outputCount + lockTime;

let weightToVSize = weight => float_of_int(weight) /. 4.;

let cost = (fee, weight) => fee |> BTC.timesFloat(weight |> weightToVSize);

let outputCost = (address, fee, network) =>
  outputWeight(address, network) |> cost(fee);

let inputCost = (nCoSigners, fee) =>
  estimateInputWeight(nCoSigners) |> cost(fee);

let minChange = inputCost;

let canPayForItself = (fee, input: Network.txInput) =>
  input.value
  |> BTC.gte(
       fee
       |> BTC.timesFloat(
            estimateInputWeight(input.nCoSigners) |> weightToVSize
          )
     );

let estimate = (outputs, inputs, fee, network) =>
  fee
  |> BTC.timesFloat(
       baseWeight
       + (
         outputs |> List.fold_left((t, o) => t + outputWeight(o, network), 0)
       )
       + (
         inputs
         |> List.fold_left(
              (t, i: Network.txInput) => t + estimateInputWeight(i.nCoSigners),
              0
            )
       )
       |> weightToVSize
     );
