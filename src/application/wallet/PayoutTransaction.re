open WalletTypes;

exception NotEnoughFunds;

type input = {
  txId: string,
  txOutputN: int,
  value: BTC.t,
  coordinates: AddressCoordinates.t,
  nCoSigners: int
};

module Fee = {
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
  let canPayForItself = (fee, input) =>
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
                (t, i) => t + estimateInputWeight(i.nCoSigners),
                0
              )
         )
         |> weightToVSize
       );
};

type t = {
  txHex: string,
  usedInputs: list(input)
};

let rec findInput = (inputs, ammountMissing, fee) =>
  switch inputs {
  | [] => None
  | [i] => Some(i)
  | [i, ...rest] =>
    i.value
    |> BTC.gte(ammountMissing |> BTC.plus(Fee.inputCost(i.nCoSigners, fee))) ?
      Some(i) : findInput(rest, ammountMissing, fee)
  };

let rec findInputs = (inputs, ammountMissing, fee, addedInputs) =>
  switch (findInput(inputs, ammountMissing, fee)) {
  | Some(i) =>
    let addedInputs = [i, ...addedInputs];
    let ammountMissing =
      ammountMissing
      |> BTC.plus(Fee.inputCost(i.nCoSigners, fee))
      |> BTC.minus(i.value);
    if (BTC.zero |> BTC.gte(ammountMissing)) {
      (addedInputs, true);
    } else {
      findInputs(
        inputs |> List.filter(input => input != i),
        ammountMissing,
        fee,
        addedInputs
      );
    };
  | None => (addedInputs, false)
  };

let addChangeOutput =
    (
      ~totalInputs,
      ~outTotal,
      ~currentFee,
      ~changeAddress: AccountKeyChain.Address.t,
      ~fee,
      ~network,
      ~txBuilder
    ) =>
  if (totalInputs
      |> BTC.gte(
           outTotal
           |> BTC.plus(currentFee)
           |> BTC.plus(Fee.outputCost(changeAddress.address, fee, network))
           |> BTC.plus(Fee.minChange(changeAddress.nCoSigners, fee))
         )) {
    let currentFee =
      currentFee
      |> BTC.plus(Fee.outputCost(changeAddress.address, fee, network));
    txBuilder
    |> Bitcoin.TxBuilder.addOutput(
         changeAddress.address,
         totalInputs
         |> BTC.minus(outTotal)
         |> BTC.minus(currentFee)
         |> BTC.toSatoshisFloat
       )
    |> ignore;
    true;
  } else {
    false;
  };

let build =
    (
      ~mandatoryInputs,
      ~reservedInputs,
      ~allInputs,
      ~destinations,
      ~fee,
      ~changeAddress: AccountKeyChain.Address.t,
      ~addresses,
      ~network
    ) => {
  open Bitcoin;
  let mandatoryInputs =
    mandatoryInputs |> List.filter(Fee.canPayForItself(fee));
  let allInputs = allInputs |> List.filter(Fee.canPayForItself(fee));
  let txB = TxBuilder.createWithNetwork(network);
  let usedInputs =
    mandatoryInputs
    |> List.map(i =>
         if (reservedInputs
             |>
             List.mem(r => r.txId == i.txId && r.txOutputN == i.txOutputN) == false) {
           txB |> TxBuilder.addInput(i.txId, i.txOutputN) |> ignore;
           Some(i);
         } else {
           None;
         }
       )
    |> List.filter(i => i |> Js.Option.isSome)
    |> List.map(Js.Option.getExn);
  let outTotal =
    destinations
    |> List.fold_left(
         (total, (address, value)) => {
           txB
           |> TxBuilder.addOutput(address, value |> BTC.toSatoshisFloat)
           |> ignore;
           total |> BTC.plus(value);
         },
         BTC.zero
       );
  let currentInputValue =
    usedInputs
    |> List.fold_left(
         (total, input) => total |> BTC.plus(input.value),
         BTC.zero
       );
  let currentFee =
    Fee.estimate(destinations |> List.map(fst), usedInputs, fee, network);
  if (currentInputValue |> BTC.gte(outTotal |> BTC.plus(currentFee))) {
    let changeAdded =
      addChangeOutput(
        ~totalInputs=currentInputValue,
        ~currentFee,
        ~changeAddress,
        ~fee,
        ~network,
        ~txBuilder=txB
      );
    (usedInputs, changeAdded);
  } else {
    let (inputs, success) =
      findInputs(allInputs, outTotal |> BTC.plus(currentFee), fee, []);
    if (success) {
      let (currentInputValue, currentFee) =
        inputs
        |> List.fold_left(
             ((inV, feeV), i) => {
               txB |> TxBuilder.addInput(i.txId, i.txOutputN) |> ignore;
               (
                 inV |> BTC.plus(i.value),
                 feeV |> BTC.plus(Fee.inputCost(i.nCoSigners, fee))
               );
             },
             (currentInputValue, currentFee)
           );
      let changeAdded =
        addChangeOutput(
          ~totalInputs=currentInputValue,
          ~currentFee,
          ~changeAddress,
          ~fee,
          ~network,
          ~txBuilder=txB
        );
      (usedInputs, changeAdded);
    } else {
      raise(NotEnoughFunds);
    };
  };
};
