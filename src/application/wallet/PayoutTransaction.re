module B = Bitcoin;

exception NotEnoughFunds;

exception NotEnoughSignatures;

exception NoSignaturesForInput;

module Fee = TransactionFee;

type input = Network.txInput;

let misthosFeePercent = 2.9;

type t = {
  txHex: string,
  usedInputs: array(input),
  misthosFeeAddress: string,
  changeAddress: option((string, Address.Coordinates.t)),
};

type summary = {
  reserved: BTC.t,
  spentWithFees: BTC.t,
  misthosFee: BTC.t,
  networkFee: BTC.t,
};

let summary =
    (network, {misthosFeeAddress, changeAddress, usedInputs, txHex}) => {
  let totalIn =
    usedInputs
    |> Array.fold_left(
         (total, input: input) => total |> BTC.plus(input.value),
         BTC.zero,
       );
  let tx = txHex |> B.Transaction.fromHex;
  let outs =
    tx##outs
    |> Array.to_list
    |> List.map(o =>
         (
           B.Address.fromOutputScript(
             o##script,
             Network.bitcoinNetwork(network),
           ),
           o##value |> Int64.of_float |> BTC.fromSatoshis,
         )
       );
  let totalOut =
    outs
    |> List.fold_left(
         (total, out) => total |> BTC.plus(out |> snd),
         BTC.zero,
       );
  let networkFee = totalIn |> BTC.minus(totalOut);
  let changeOut =
    changeAddress
    |> Utils.mapOption(((changeAddress, _)) =>
         outs |> List.find(((a, _)) => a == changeAddress) |> snd
       )
    |> Js.Option.getWithDefault(BTC.zero);
  let misthosFee =
    outs |> List.find(((a, _)) => a == misthosFeeAddress) |> snd;
  {
    reserved: totalIn,
    spentWithFees: totalOut |> BTC.plus(networkFee) |> BTC.minus(changeOut),
    misthosFee,
    networkFee,
  };
};

let txInputForChangeAddress =
    (~txId, accountKeyChains, network, {changeAddress, txHex}) =>
  changeAddress
  |> Utils.mapOption(((address, coordinates)) => {
       let keyChain =
         accountKeyChains
         |> AccountKeyChain.Collection.lookup(
              coordinates |> Address.Coordinates.accountIdx,
              coordinates |> Address.Coordinates.keyChainIdent,
            );
       let tx = B.Transaction.fromHex(txHex);
       let (idx, value) =
         tx##outs
         |> Array.to_list
         |> List.mapi((i, out) =>
              B.Address.fromOutputScript(
                out##script,
                network |> Network.bitcoinNetwork,
              )
              == address ?
                Some((i, BTC.fromSatoshisFloat(out##value))) : None
            )
         |> List.find(Js.Option.isSome)
         |> Js.Option.getExn;
       Network.{
         txId,
         txOutputN: idx,
         value,
         nCoSigners: keyChain.nCoSigners,
         nPubKeys: keyChain.custodianKeyChains |> List.length,
         address,
         coordinates,
       };
     });

let encode = payout =>
  Json.Encode.(
    object_([
      ("txHex", string(payout.txHex)),
      ("usedInputs", array(Network.encodeInput, payout.usedInputs)),
      ("misthosFeeAddress", string(payout.misthosFeeAddress)),
      (
        "changeAddress",
        nullable(
          tuple2(string, Address.Coordinates.encode),
          payout.changeAddress,
        ),
      ),
    ])
  );

let decode = raw =>
  Json.Decode.{
    txHex: raw |> field("txHex", string),
    usedInputs: raw |> field("usedInputs", array(Network.decodeInput)),
    misthosFeeAddress: raw |> field("misthosFeeAddress", string),
    changeAddress:
      raw
      |> field(
           "changeAddress",
           optional(tuple2(string, Address.Coordinates.decode)),
         ),
  };

type signResult =
  | Signed(t)
  | NotSigned;

let getSignedExn = result =>
  switch (result) {
  | Signed(unwrapped) => unwrapped
  | _ => %assert
         "signResult"
  };

let signPayout =
    (
      ~ventureId,
      ~userId,
      ~masterKeyChain: B.HDNode.t,
      ~accountKeyChains: AccountKeyChain.Collection.t,
      ~payoutTx as payout: t,
      ~network: Network.t,
    ) => {
  let txB =
    B.TxBuilder.fromTransactionWithNetwork(
      B.Transaction.fromHex(payout.txHex),
      network |> Network.bitcoinNetwork,
    );
  let signed =
    payout.usedInputs
    |> Array.mapi((idx, input: input) => {
         let inputs = txB##inputs;
         let txBInput = inputs[idx];
         let needsSigning =
           switch (txBInput##signatures |> Js.Nullable.toOption) {
           | Some(signatures) =>
             signatures
             |> Array.to_list
             |> List.filter(s =>
                  s |> Js.Nullable.toOption |> Js.Option.isSome
                )
             |> List.length < input.nCoSigners
           | None => true
           };
         if (needsSigning) {
           try (
             {
               let custodianPubChain =
                 (
                   accountKeyChains
                   |> AccountKeyChain.Collection.lookup(
                        input.coordinates |> Address.Coordinates.accountIdx,
                        input.coordinates |> Address.Coordinates.keyChainIdent,
                      )
                 ).
                   custodianKeyChains
                 |> List.assoc(userId);
               let custodianKeyChain =
                 CustodianKeyChain.make(
                   ~ventureId,
                   ~accountIdx=
                     CustodianKeyChain.accountIdx(custodianPubChain),
                   ~keyChainIdx=
                     CustodianKeyChain.keyChainIdx(custodianPubChain),
                   ~masterKeyChain,
                 );
               let (coSignerIdx, chainIdx, addressIdx) = (
                 input.coordinates |> Address.Coordinates.coSignerIdx,
                 input.coordinates |> Address.Coordinates.chainIdx,
                 input.coordinates |> Address.Coordinates.addressIdx,
               );
               let keyPair =
                 custodianKeyChain
                 |> CustodianKeyChain.getSigningKey(
                      coSignerIdx,
                      chainIdx,
                      addressIdx,
                    );
               let address: Address.t =
                 accountKeyChains |> Address.find(input.coordinates);
               txB
               |> B.TxBuilder.signSegwit(
                    idx,
                    keyPair,
                    ~redeemScript=address.redeemScript |> Utils.bufFromHex,
                    ~witnessValue=input.value |> BTC.toSatoshisFloat,
                    ~witnessScript=address.witnessScript |> Utils.bufFromHex,
                  );
               true;
             }
           ) {
           | Not_found => false
           };
         } else {
           false;
         };
       });
  signed |> Js.Array.find(s => s) |> Js.Option.isSome ?
    Signed({
      ...payout,
      txHex: txB |> B.TxBuilder.buildIncomplete |> B.Transaction.toHex,
    }) :
    NotSigned;
};

let rec findInput = (inputs, ammountMissing, fee) =>
  switch (inputs) {
  | [] => None
  | [i] => Some(i)
  | [(i: input), ...rest] =>
    i.value
    |> BTC.gte(
         ammountMissing
         |> BTC.plus(Fee.inputCost(i.nCoSigners, i.nPubKeys, fee)),
       ) ?
      Some(i) : findInput(rest, ammountMissing, fee)
  };

let rec findInputs = (inputs, ammountMissing, fee, addedInputs) =>
  switch (findInput(inputs, ammountMissing, fee)) {
  | Some(i) =>
    let addedInputs = [i, ...addedInputs];
    let ammountMissing =
      ammountMissing
      |> BTC.plus(Fee.inputCost(i.nCoSigners, i.nPubKeys, fee))
      |> BTC.minus(i.value);
    if (BTC.zero |> BTC.gte(ammountMissing)) {
      (addedInputs, true);
    } else {
      findInputs(
        inputs |> List.filter(input => input != i),
        ammountMissing,
        fee,
        addedInputs,
      );
    };
  | None => (addedInputs, false)
  };

let addChangeOutput =
    (
      ~totalInputs,
      ~outTotal,
      ~currentFee,
      ~changeAddress: Address.t,
      ~fee,
      ~network,
      ~txBuilder,
    ) =>
  if (totalInputs
      |> BTC.gte(
           outTotal
           |> BTC.plus(currentFee)
           |> BTC.plus(
                Fee.outputCost(
                  changeAddress.displayAddress,
                  fee,
                  network |> Network.bitcoinNetwork,
                ),
              )
           |> BTC.plus(
                Fee.minChange(
                  changeAddress.nCoSigners,
                  changeAddress.nPubKeys,
                  fee,
                ),
              ),
         )) {
    let currentFee =
      currentFee
      |> BTC.plus(
           Fee.outputCost(
             changeAddress.displayAddress,
             fee,
             network |> Network.bitcoinNetwork,
           ),
         );
    txBuilder
    |> B.TxBuilder.addOutput(
         changeAddress.displayAddress,
         totalInputs
         |> BTC.minus(outTotal)
         |> BTC.minus(currentFee)
         |> BTC.toSatoshisFloat,
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
      ~changeAddress: Address.t,
      ~network,
    ) => {
  let mandatoryInputs =
    mandatoryInputs |. Belt.Set.keep(Fee.canPayForItself(satsPerByte));
  let allInputs =
    allInputs
    |. Belt.Set.keep(Fee.canPayForItself(satsPerByte))
    |. Belt.Set.diff(mandatoryInputs)
    |> Belt.Set.toList
    |> List.sort((i1: Network.txInput, i2: Network.txInput) =>
         i1.value |> BTC.comparedTo(i2.value)
       );
  let txB = B.TxBuilder.createWithNetwork(network |> Network.bitcoinNetwork);
  let usedInputs =
    mandatoryInputs
    |> Belt.Set.toList
    |> List.map((i: input) =>
         (txB |> B.TxBuilder.addInput(i.txId, i.txOutputN), i)
       );
  let outTotalWithoutFee =
    destinations
    |> List.fold_left(
         (total, (address, value)) => {
           txB
           |> B.TxBuilder.addOutput(address, value |> BTC.toSatoshisFloat)
           |> ignore;
           total |> BTC.plus(value);
         },
         BTC.zero,
       );
  let misthosFee =
    outTotalWithoutFee |> BTC.timesRounded(misthosFeePercent /. 100.);
  let misthosFeeAddress = Network.incomeAddress(network);
  txB
  |> B.TxBuilder.addOutput(
       misthosFeeAddress,
       misthosFee |> BTC.toSatoshisFloat,
     )
  |> ignore;
  let outTotal = outTotalWithoutFee |> BTC.plus(misthosFee);
  let currentInputValue =
    usedInputs
    |> List.fold_left(
         (total, (_, input: input)) => total |> BTC.plus(input.value),
         BTC.zero,
       );
  let currentFee =
    Fee.estimate(
      destinations |> List.map(fst),
      usedInputs |> List.map(snd),
      satsPerByte,
      network |> Network.bitcoinNetwork,
    );
  if (currentInputValue |> BTC.gte(outTotal |> BTC.plus(currentFee))) {
    let withChange =
      addChangeOutput(
        ~totalInputs=currentInputValue,
        ~outTotal,
        ~currentFee,
        ~changeAddress,
        ~fee=satsPerByte,
        ~network,
        ~txBuilder=txB,
      );
    {
      usedInputs:
        usedInputs
        |> Array.of_list
        |> Js.Array.sortInPlaceWith(((idxA, _), (idxB, _)) =>
             compare(idxA, idxB)
           )
        |> Array.map(((_, input)) => input),
      txHex: txB |> B.TxBuilder.buildIncomplete |> B.Transaction.toHex,
      misthosFeeAddress,
      changeAddress:
        withChange ?
          Some((changeAddress.displayAddress, changeAddress.coordinates)) :
          None,
    };
  } else {
    let (inputs, success) =
      findInputs(
        allInputs,
        outTotal |> BTC.plus(currentFee) |> BTC.minus(currentInputValue),
        satsPerByte,
        [],
      );
    if (success) {
      let (currentInputValue, currentFee, usedInputs) =
        inputs
        |> List.fold_left(
             ((inV, feeV, usedInputs), i: input) => (
               inV |> BTC.plus(i.value),
               feeV
               |> BTC.plus(
                    Fee.inputCost(i.nCoSigners, i.nPubKeys, satsPerByte),
                  ),
               [
                 (txB |> B.TxBuilder.addInput(i.txId, i.txOutputN), i),
                 ...usedInputs,
               ],
             ),
             (currentInputValue, currentFee, usedInputs),
           );
      let withChange =
        addChangeOutput(
          ~totalInputs=currentInputValue,
          ~outTotal,
          ~currentFee,
          ~changeAddress,
          ~fee=satsPerByte,
          ~network,
          ~txBuilder=txB,
        );
      {
        usedInputs:
          usedInputs
          |> Array.of_list
          |> Js.Array.sortInPlaceWith(((idxA, _), (idxB, _)) =>
               compare(idxA, idxB)
             )
          |> Array.map(((_, input)) => input),
        txHex: txB |> B.TxBuilder.buildIncomplete |> B.Transaction.toHex,
        misthosFeeAddress,
        changeAddress:
          withChange ?
            Some((changeAddress.displayAddress, changeAddress.coordinates)) :
            None,
      };
    } else {
      raise(NotEnoughFunds);
    };
  };
};

let max =
    (~allInputs, ~targetDestination, ~destinations, ~satsPerByte, ~network) => {
  open Belt;
  let inputs =
    allInputs
    |> Set.toList
    |. List.keepMapU((. input) =>
         TransactionFee.canPayForItself(satsPerByte, input) ?
           Some(input) : None
       );
  let outputs =
    if (targetDestination != "") {
      [
        (targetDestination, BTC.zero),
        (Network.incomeAddress(network), BTC.zero),
        ...destinations,
      ];
    } else {
      [(Network.incomeAddress(network), BTC.zero), ...destinations];
    };
  let fee =
    Fee.estimate(
      outputs |. List.map(fst),
      inputs,
      satsPerByte,
      network |> Network.bitcoinNetwork,
    );
  let totalInputValue =
    inputs
    |. List.reduce(BTC.zero, (res, input) => res |> BTC.plus(input.value));
  let totalOutValue =
    destinations
    |. List.reduce(BTC.zero, (res, (_, outVal)) => res |> BTC.plus(outVal));
  let totalOutMisthosFee =
    totalOutValue |> BTC.timesRounded(misthosFeePercent /. 100.);
  let rest = totalInputValue |> BTC.minus(totalOutValue |> BTC.plus(fee));
  rest
  |> BTC.dividedByRounded(1. +. misthosFeePercent /. 100.)
  |> BTC.minus(totalOutMisthosFee);
};

let rec findSignatures = (allSigs, needed, foundSigIdxs, foundSigs, network) =>
  switch (needed, allSigs) {
  | (0, _)
  | (_, []) => foundSigs
  | (_, [None, ...otherSigs]) =>
    findSignatures(otherSigs, needed, foundSigIdxs, foundSigs, network)
  | (_, [Some(signatures), ...otherSigs]) =>
    try (
      {
        let foundSig =
          signatures
          |> Array.mapi((i, sigBuf) => (i, sigBuf))
          |> Array.to_list
          |> List.find(((i, signature)) =>
               Js.Nullable.test(signature) == false
               && foundSigIdxs
               |> List.mem(i) == false
             );
        let foundSigs = [foundSig, ...foundSigs];
        if (needed == 1) {
          foundSigs;
        } else {
          findSignatures(
            allSigs,
            needed - 1,
            [fst(foundSig), ...foundSigIdxs],
            foundSigs,
            network,
          );
        };
      }
    ) {
    | Not_found =>
      findSignatures(otherSigs, needed, foundSigIdxs, foundSigs, network)
    }
  };

let finalize = (signedTransactions, network) =>
  switch (signedTransactions) {
  | [{txHex, usedInputs}, ...moreSignedTransactions] =>
    let txB =
      B.TxBuilder.fromTransactionWithNetwork(
        txHex |> B.Transaction.fromHex,
        network |> Network.bitcoinNetwork,
      );
    let inputs = txB##inputs;
    let otherInputs =
      moreSignedTransactions
      |> List.map(({txHex}: t) =>
           B.TxBuilder.fromTransactionWithNetwork(
             txHex |> B.Transaction.fromHex,
             network |> Network.bitcoinNetwork,
           )##inputs
         );
    usedInputs
    |> Array.iteri((inputIdx, {nCoSigners}: input) => {
         let testInput = inputs[inputIdx];
         inputs[inputIdx] = (
           switch (testInput##signatures |> Js.Nullable.toOption) {
           | Some(_) => inputs[inputIdx]
           | None =>
             let inputs =
               try (
                 otherInputs
                 |> List.find(ins => {
                      let input = ins[inputIdx];
                      input##signatures
                      |> Js.Nullable.toOption
                      |> Js.Option.isSome;
                    })
               ) {
               | Not_found => raise(NoSignaturesForInput)
               };
             inputs[inputIdx];
           }
         );
         let input = inputs[inputIdx];
         let signatures =
           input##signatures |> Js.Nullable.toOption |> Js.Option.getExn;
         let existing =
           signatures
           |> Array.mapi((i, sigBuf) =>
                switch (sigBuf |> Js.Nullable.toOption) {
                | Some(_) => Some(i)
                | None => None
                }
              )
           |> Array.to_list
           |> List.filter(Js.Option.isSome)
           |> List.map(i => Js.Option.getExn(i));
         let total =
           findSignatures(
             otherInputs
             |> List.map(ins => {
                  let input = ins[inputIdx];
                  input##signatures |> Js.Nullable.toOption;
                }),
             nCoSigners - (existing |> List.length),
             existing,
             [],
             network,
           )
           |> List.fold_left(
                (res, (sigIdx, signature)) => {
                  signatures[sigIdx] = signature;
                  res + 1;
                },
                existing |> List.length,
              );
         if (total != nCoSigners) {
           raise(NotEnoughSignatures);
         };
       });
    txB |> B.TxBuilder.build;
  | _ => %assert
         "finalize"
  };
