open WalletTypes;

exception NotEnoughFunds;

type input = Network.txInput;

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
  let canPayForItself = (fee, input: input) =>
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
                (t, (_, i: input)) => t + estimateInputWeight(i.nCoSigners),
                0
              )
         )
         |> weightToVSize
       );
};

type t = {
  txHex: string,
  usedInputs: list((int, input))
};

let signPayout =
    (
      ~ventureId,
      ~session: Session.Data.t,
      ~accountKeyChains:
         list((accountIdx, list((accountKeyChainIdx, AccountKeyChain.t)))),
      ~payout: t,
      ~network: Bitcoin.Networks.t
    ) => {
  open Bitcoin;
  let txB =
    TxBuilder.fromTransactionWithNetwork(
      Transaction.fromHex(payout.txHex),
      network
    );
  payout.usedInputs
  |> List.iter(((idx, input: input)) =>
       try {
         let custodianPubChain =
           (
             accountKeyChains
             |> AddressCoordinates.lookupKeyChain(input.coordinates)
           ).
             custodianKeyChains
           |> List.assoc(session.userId);
         let custodianKeyChain =
           CustodianKeyChain.make(
             ~ventureId,
             ~accountIdx=CustodianKeyChain.accountIdx(custodianPubChain),
             ~keyChainIdx=CustodianKeyChain.keyChainIdx(custodianPubChain),
             ~masterKeyChain=session.masterKeyChain
           );
         let keyPair =
           custodianKeyChain
           |> CustodianKeyChain.getSigningKey(input.coordinates);
         let address: AccountKeyChain.Address.t =
           accountKeyChains |> AccountKeyChain.find(input.coordinates);
         txB
         |> TxBuilder.signSegwit(
              idx,
              keyPair,
              ~redeemScript=address.redeemScript |> Utils.bufFromHex,
              ~witnessValue=input.value |> BTC.toSatoshisFloat,
              ~witnessScript=address.witnessScript |> Utils.bufFromHex
            );
       } {
       | Not_found => ()
       }
     );
  {...payout, txHex: txB |> TxBuilder.buildIncomplete |> Transaction.toHex};
};

let rec findInput = (inputs, ammountMissing, fee) =>
  switch inputs {
  | [] => None
  | [i] => Some(i)
  | [(i: input), ...rest] =>
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
      ~allInputs,
      ~destinations,
      ~satsPerByte,
      ~changeAddress: AccountKeyChain.Address.t,
      ~network
    ) => {
  open Bitcoin;
  let mandatoryInputs =
    mandatoryInputs |> List.filter(Fee.canPayForItself(satsPerByte));
  let allInputs = allInputs |> List.filter(Fee.canPayForItself(satsPerByte));
  let txB = TxBuilder.createWithNetwork(network);
  let usedInputs =
    mandatoryInputs
    |> List.map((i: input) =>
         (txB |> TxBuilder.addInput(i.txId, i.txOutputN), i)
       );
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
         (total, (_, input: input)) => total |> BTC.plus(input.value),
         BTC.zero
       );
  let currentFee =
    Fee.estimate(
      destinations |> List.map(fst),
      usedInputs,
      satsPerByte,
      network
    );
  if (currentInputValue |> BTC.gte(outTotal |> BTC.plus(currentFee))) {
    let changeAdded =
      addChangeOutput(
        ~totalInputs=currentInputValue,
        ~outTotal,
        ~currentFee,
        ~changeAddress,
        ~fee=satsPerByte,
        ~network,
        ~txBuilder=txB
      );
    (
      {
        usedInputs,
        txHex: txB |> TxBuilder.buildIncomplete |> Transaction.toHex
      },
      changeAdded
    );
  } else {
    let (inputs, success) =
      findInputs(allInputs, outTotal |> BTC.plus(currentFee), satsPerByte, []);
    if (success) {
      let (currentInputValue, currentFee, usedInputs) =
        inputs
        |> List.fold_left(
             ((inV, feeV, usedInputs), i: input) => (
               inV |> BTC.plus(i.value),
               feeV |> BTC.plus(Fee.inputCost(i.nCoSigners, satsPerByte)),
               [
                 (txB |> TxBuilder.addInput(i.txId, i.txOutputN), i),
                 ...usedInputs
               ]
             ),
             (currentInputValue, currentFee, usedInputs)
           );
      let changeAdded =
        addChangeOutput(
          ~totalInputs=currentInputValue,
          ~outTotal,
          ~currentFee,
          ~changeAddress,
          ~fee=satsPerByte,
          ~network,
          ~txBuilder=txB
        );
      (
        {
          usedInputs,
          txHex: txB |> TxBuilder.buildIncomplete |> Transaction.toHex
        },
        changeAdded
      );
    } else {
      raise(NotEnoughFunds);
    };
  };
};
