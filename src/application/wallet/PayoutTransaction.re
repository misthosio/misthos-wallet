open Bitcoin;

open WalletTypes;

exception NotEnoughFunds;

exception NotEnoughSignatures;

module Fee = TransactionFee;

type input = Network.txInput;

type t = {
  txHex: string,
  usedInputs: list((int, input)),
  withChange: bool,
};

type summary = {
  reserved: BTC.t,
  spent: BTC.t,
  fee: BTC.t,
};

let summary = ({withChange, usedInputs, txHex}) => {
  let totalIn =
    usedInputs
    |> List.fold_left(
         (total, input) => total |> BTC.plus((snd(input): input).value),
         BTC.zero,
       );
  let tx = txHex |> Transaction.fromHex;
  let outs =
    tx##outs
    |> Array.to_list
    |> List.map(o => o##value |> Int64.of_float |> BTC.fromSatoshis);
  let totalOut =
    outs |> List.fold_left((total, out) => total |> BTC.plus(out), BTC.zero);
  let fee = totalIn |> BTC.minus(totalOut);
  let changeOut = withChange ? outs |> List.rev |> List.hd : BTC.zero;
  {
    reserved: totalIn,
    spent: totalOut |> BTC.plus(fee) |> BTC.minus(changeOut),
    fee,
  };
};

let encode = payout =>
  Json.Encode.(
    object_([
      ("txHex", string(payout.txHex)),
      (
        "usedInputs",
        list(pair(int, Network.encodeInput), payout.usedInputs),
      ),
      ("withChange", bool(payout.withChange)),
    ])
  );

let decode = raw =>
  Json.Decode.{
    txHex: raw |> field("txHex", string),
    usedInputs:
      raw |> field("usedInputs", list(pair(int, Network.decodeInput))),
    withChange: raw |> field("withChange", bool),
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
      ~masterKeyChain: HDNode.t,
      ~accountKeyChains:
         list((accountIdx, list((accountKeyChainIdx, AccountKeyChain.t)))),
      ~payoutTx as payout: t,
      ~network: Network.t,
    ) => {
  let txB =
    TxBuilder.fromTransactionWithNetwork(
      Transaction.fromHex(payout.txHex),
      network |> Network.bitcoinNetwork,
    );
  let signed =
    payout.usedInputs
    |> List.fold_left(
         (signed, (idx, input: input)) =>
           try (
             {
               let custodianPubChain =
                 (
                   accountKeyChains
                   |> AccountKeyChain.lookupKeyChain(input.coordinates)
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
               let (chainIdx, addressIdx) = (
                 input.coordinates
                 |> AccountKeyChain.Address.Coordinates.chainIdx,
                 input.coordinates
                 |> AccountKeyChain.Address.Coordinates.addressIdx,
               );
               let keyPair =
                 custodianKeyChain
                 |> CustodianKeyChain.getSigningKey(chainIdx, addressIdx);
               let address: AccountKeyChain.Address.t =
                 accountKeyChains |> AccountKeyChain.find(input.coordinates);
               txB
               |> TxBuilder.signSegwit(
                    idx,
                    keyPair,
                    ~redeemScript=address.redeemScript |> Utils.bufFromHex,
                    ~witnessValue=input.value |> BTC.toSatoshisFloat,
                    ~witnessScript=address.witnessScript |> Utils.bufFromHex,
                  );
               true;
             }
           ) {
           | Not_found => signed
           },
         false,
       );
  signed ?
    Signed({
      ...payout,
      txHex: txB |> TxBuilder.buildIncomplete |> Transaction.toHex,
    }) :
    NotSigned;
};

let rec findInput = (inputs, ammountMissing, fee) =>
  switch (inputs) {
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
      ~changeAddress: AccountKeyChain.Address.t,
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
                  changeAddress.address,
                  fee,
                  network |> Network.bitcoinNetwork,
                ),
              )
           |> BTC.plus(Fee.minChange(changeAddress.nCoSigners, fee)),
         )) {
    let currentFee =
      currentFee
      |> BTC.plus(
           Fee.outputCost(
             changeAddress.address,
             fee,
             network |> Network.bitcoinNetwork,
           ),
         );
    txBuilder
    |> TxBuilder.addOutput(
         changeAddress.address,
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

type buildResult =
  | WithChangeAddress(t)
  | WithoutChangeAddress(t);

let build =
    (
      ~mandatoryInputs,
      ~allInputs,
      ~destinations,
      ~satsPerByte,
      ~changeAddress: AccountKeyChain.Address.t,
      ~network,
    ) => {
  let mandatoryInputs =
    mandatoryInputs |> List.filter(Fee.canPayForItself(satsPerByte));
  let allInputs =
    allInputs
    |> List.filter(Fee.canPayForItself(satsPerByte))
    |> List.filter(input => mandatoryInputs |> List.mem(input) == false)
    |> List.sort((i1: Network.txInput, i2: Network.txInput) =>
         i1.value |> BTC.comparedTo(i2.value)
       );
  let txB = TxBuilder.createWithNetwork(network |> Network.bitcoinNetwork);
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
         BTC.zero,
       );
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
    let result = {
      usedInputs,
      txHex: txB |> TxBuilder.buildIncomplete |> Transaction.toHex,
      withChange,
    };
    withChange ? WithChangeAddress(result) : WithoutChangeAddress(result);
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
               feeV |> BTC.plus(Fee.inputCost(i.nCoSigners, satsPerByte)),
               [
                 (txB |> TxBuilder.addInput(i.txId, i.txOutputN), i),
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
      let result = {
        usedInputs,
        txHex: txB |> TxBuilder.buildIncomplete |> Transaction.toHex,
        withChange,
      };
      withChange ? WithChangeAddress(result) : WithoutChangeAddress(result);
    } else {
      raise(NotEnoughFunds);
    };
  };
};

let rec findSignatures = (allSigs, needed, foundSigIdxs, foundSigs, network) =>
  switch (needed, allSigs) {
  | (0, _)
  | (_, []) => foundSigs
  | (_, [signatures, ...otherSigs]) =>
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
      TxBuilder.fromTransactionWithNetwork(
        txHex |> Transaction.fromHex,
        network |> Network.bitcoinNetwork,
      );
    let inputs = txB##inputs;
    let otherInputs =
      moreSignedTransactions
      |> List.map(({txHex}: t) =>
           TxBuilder.fromTransactionWithNetwork(
             txHex |> Transaction.fromHex,
             network |> Network.bitcoinNetwork,
           )##inputs
         );
    usedInputs
    |> List.iter(((inputIdx, {nCoSigners}: input)) => {
         let input = inputs[inputIdx];
         let signatures = input##signatures;
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
                  input##signatures;
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
    txB |> TxBuilder.build;
  | _ => %assert
         "finalize"
  };
