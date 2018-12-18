module B = Bitcoin;
open PrimitiveTypes;

exception NotEnoughFunds;

exception NotEnoughSignatures;

exception NoSignaturesForInput;

module Fee = TransactionFee;

type input = Network.txInput;

type t = {
  txHex: string,
  usedInputs: array(input),
  misthosFeeAddress: string,
  changeAddress: option(Address.t),
};

type summary = {
  reserved: BTC.t,
  destinations: list((string, BTC.t)),
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
  let cAddress =
    changeAddress
    |> Utils.mapOption((a: Address.t) => a.displayAddress)
    |> Js.Option.getWithDefault("");
  let destinations =
    Belt.(
      outs->(
              List.keepMapU((. (address, value)) =>
                if (address != misthosFeeAddress && address != cAddress) {
                  Some((address, value));
                } else {
                  None;
                }
              )
            )
    );
  let networkFee = totalIn->(BTC.minus(totalOut));
  let changeOut =
    changeAddress
    |> Utils.mapOption((changeAddress: Address.t) =>
         outs
         |> List.find(((a, _)) => a == changeAddress.displayAddress)
         |> snd
       )
    |> Js.Option.getWithDefault(BTC.zero);
  let misthosFee =
    try (outs |> List.find(((a, _)) => a == misthosFeeAddress) |> snd) {
    | Not_found => BTC.zero
    };
  {
    reserved: totalIn,
    destinations,
    spentWithFees:
      (totalOut |> BTC.plus(networkFee))->(BTC.minus(changeOut)),
    misthosFee,
    networkFee,
  };
};

let txInputForChangeAddress = (~txId, network, {changeAddress, txHex}) =>
  changeAddress
  |> Utils.mapOption((address: Address.t) => {
       let tx = B.Transaction.fromHex(txHex);
       let (idx, value) =
         tx##outs
         |> Array.to_list
         |> List.mapi((i, out) =>
              B.Address.fromOutputScript(
                out##script,
                network |> Network.bitcoinNetwork,
              )
              == address.displayAddress ?
                Some((i, BTC.fromSatoshisFloat(out##value))) : None
            )
         |> List.find(Js.Option.isSome)
         |> Js.Option.getExn;
       Network.{
         txId,
         txOutputN: idx,
         value,
         nCoSigners: address.nCoSigners,
         nPubKeys: address.nPubKeys,
         address: address.displayAddress,
         coordinates: address.coordinates,
         sequence: address.sequence,
         unlocked: false,
       };
     });

let encode = payout =>
  Json.Encode.(
    object_([
      ("txHex", string(payout.txHex)),
      ("usedInputs", array(Network.encodeInput, payout.usedInputs)),
      ("misthosFeeAddress", string(payout.misthosFeeAddress)),
      ("changeAddress", nullable(Address.encode, payout.changeAddress)),
    ])
  );

let decode = raw =>
  Json.Decode.{
    txHex: raw |> field("txHex", string),
    usedInputs: raw |> field("usedInputs", array(Network.decodeInput)),
    misthosFeeAddress: raw |> field("misthosFeeAddress", string),
    changeAddress: raw |> field("changeAddress", optional(Address.decode)),
  };

type signResult =
  | Signed(t)
  | NotSigned;

let getSignedExn = result =>
  switch (result) {
  | Signed(unwrapped) => unwrapped
  | _ =>
    %assert
    "signResult"
  };

let signPayout =
    (
      ~ventureId,
      ~userId,
      ~masterKeyChain: B.HDNode.t,
      ~accountKeyChains: AccountKeyChain.Collection.t,
      ~payoutTx as payout: t,
      ~signatures,
    ) => {
  let txW = ref(TxWrapper.make(payout.txHex));
  let signed =
    payout.usedInputs
    |> Array.mapi((idx, input: input) => {
         let needsSigning =
           txW^ |> TxWrapper.needsSigning(idx, input.nCoSigners);
         if (needsSigning) {
           try (
             {
               let accountKeyChain =
                 accountKeyChains
                 |> AccountKeyChain.Collection.lookup(
                      input.coordinates |> Address.Coordinates.accountIdx,
                      input.coordinates |> Address.Coordinates.keyChainIdent,
                    );
               let custodianPubChain =
                 accountKeyChain.custodianKeyChains |> List.assoc(userId);
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
               txW :=
                 txW^
                 |> TxWrapper.sign(
                      idx,
                      keyPair,
                      ~nCustodians=
                        accountKeyChain.custodianKeyChains |> List.length,
                      ~redeemScript=address.redeemScript,
                      ~witnessValue=input.value,
                      ~witnessScript=address.witnessScript,
                      ~signature=
                        signatures->(Belt.Array.get(idx))
                        |> Js.Option.getWithDefault(None),
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
    Signed({...payout, txHex: txW^.tx |> B.Transaction.toHex}) : NotSigned;
};

let rec findInput = (inputs, ammountMissing, fee) =>
  switch (inputs) {
  | [] => None
  | [i] => Some(i)
  | [(i: input), ...rest] =>
    i.value
    ->(
        BTC.gte(
          ammountMissing
          |> BTC.plus(
               Fee.inputCost(
                 ~withDms=i.sequence |> Js.Option.isSome,
                 ~unlocked=i.unlocked,
                 i.nCoSigners,
                 i.nPubKeys,
                 fee,
               ),
             ),
        )
      ) ?
      Some(i) : findInput(rest, ammountMissing, fee)
  };

let rec findInputs = (inputs, ammountMissing, fee, addedInputs) =>
  switch (findInput(inputs, ammountMissing, fee)) {
  | Some(i) =>
    let addedInputs = [i, ...addedInputs];
    let ammountMissing =
      (
        ammountMissing
        |> BTC.plus(
             Fee.inputCost(
               ~withDms=i.sequence |> Js.Option.isSome,
               ~unlocked=i.unlocked,
               i.nCoSigners,
               i.nPubKeys,
               fee,
             ),
           )
      )
      ->(BTC.minus(i.value));
    if (BTC.zero->(BTC.gte(ammountMissing))) {
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
  if (totalInputs->(
                     BTC.gte(
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
                              ~withDms=
                                changeAddress.sequence |> Js.Option.isSome,
                              ~unlocked=false,
                              changeAddress.nCoSigners,
                              changeAddress.nPubKeys,
                              fee,
                            ),
                          ),
                     )
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
    txBuilder->(
                 B.TxBuilder.addOutput(
                   changeAddress.displayAddress,
                   totalInputs
                   ->(BTC.minus(outTotal))
                   ->(BTC.minus(currentFee))
                   |> BTC.toSatoshisFloat,
                 )
               )
    |> ignore;
    true;
  } else {
    false;
  };

let build =
    (
      ~optionalInputs,
      ~mandatoryInputs,
      ~unlockedInputs,
      ~destinations,
      ~satsPerByte,
      ~changeAddress: Address.t,
      ~network,
    ) => {
  let unlockedInputs =
    unlockedInputs->(Belt.Set.keep(Fee.canPayForItself(satsPerByte)));
  let mandatoryInputs =
    mandatoryInputs
    ->(Belt.Set.keep(Fee.canPayForItself(satsPerByte)))
    ->(Belt.Set.union(unlockedInputs));
  let optionalInputs =
    optionalInputs
    ->(Belt.Set.keep(Fee.canPayForItself(satsPerByte)))
    ->(Belt.Set.diff(unlockedInputs))
    |> Belt.Set.toList
    |> List.sort((i1: Network.txInput, i2: Network.txInput) =>
         i1.value->(BTC.comparedTo(i2.value))
       );
  let txB = B.TxBuilder.createWithNetwork(network |> Network.bitcoinNetwork);
  let usedInputs =
    mandatoryInputs
    |> Belt.Set.toList
    |> List.map((i: input) =>
         (
           i.unlocked ?
             txB->(
                    B.TxBuilder.addInputWithSequence(
                      i.txId,
                      i.txOutputN,
                      i.sequence |> Js.Option.getExn,
                    )
                  ) :
             txB->(B.TxBuilder.addInput(i.txId, i.txOutputN)),
           i,
         )
       );
  let outTotal =
    destinations
    |> List.fold_left(
         (total, (address, value)) => {
           txB->(B.TxBuilder.addOutput(address, value |> BTC.toSatoshisFloat))
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
  if (currentInputValue->(BTC.gte(outTotal |> BTC.plus(currentFee)))) {
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
      misthosFeeAddress: "",
      changeAddress: withChange ? Some(changeAddress) : None,
    };
  } else {
    let (inputs, success) =
      findInputs(
        optionalInputs,
        (outTotal |> BTC.plus(currentFee))->(BTC.minus(currentInputValue)),
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
                    Fee.inputCost(
                      ~withDms=i.sequence |> Js.Option.isSome,
                      ~unlocked=i.unlocked,
                      i.nCoSigners,
                      i.nPubKeys,
                      satsPerByte,
                    ),
                  ),
               [
                 (txB->(B.TxBuilder.addInput(i.txId, i.txOutputN)), i),
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
        misthosFeeAddress: "",
        changeAddress: withChange ? Some(changeAddress) : None,
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
    (allInputs |> Set.toList)
    ->(
        List.keepMapU((. input) =>
          TransactionFee.canPayForItself(satsPerByte, input) ?
            Some(input) : None
        )
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
      outputs->(List.map(fst)),
      inputs,
      satsPerByte,
      network |> Network.bitcoinNetwork,
    );
  let totalInputValue =
    inputs->(
              List.reduce(BTC.zero, (res, input) =>
                res |> BTC.plus(input.value)
              )
            );
  let totalOutValue =
    destinations->(
                    List.reduce(BTC.zero, (res, (_, outVal)) =>
                      res |> BTC.plus(outVal)
                    )
                  );
  totalInputValue->(BTC.minus(totalOutValue |> BTC.plus(fee)));
};

let finalize = signedTransactions => {
  open Belt;
  let signedTransactions =
    signedTransactions->(
                          List.sortU((. {txHex: hexA}, {txHex: hexB}) =>
                            compare(hexA, hexB)
                          )
                        );
  let wrappers =
    signedTransactions->(List.mapU((. {txHex}) => txHex |> TxWrapper.make));
  let res =
    (
      switch (wrappers |> List.head, wrappers |> List.tail) {
      | (Some(head), Some(rest)) =>
        rest->(List.reduce(head, TxWrapper.merge))
      | (Some(head), _) => head
      | _ =>
        %assert
        "finalize"
      }
    )
    |> TxWrapper.finalize((signedTransactions |> List.headExn).usedInputs);
  switch (res) {
  | Ok(tx) => tx
  | NotEnoughSignatures => raise(NotEnoughSignatures)
  };
};

type inputMissingSigs = {
  custodians: UserId.set,
  sigs: int,
};
type missingSignatures = {
  mandatory: UserId.set,
  additional: UserId.set,
};
let missingSignatures =
    (
      ~currentCustodians,
      ~custodiansThatSigned,
      keyChains,
      {txHex, usedInputs},
    ) => {
  open Belt;
  let txWrapper = TxWrapper.make(txHex);
  let missingSigs =
    usedInputs->(
                  Array.mapWithIndexU((. idx, input: Network.txInput) => {
                    let keyChain =
                      keyChains
                      |> AccountKeyChain.Collection.lookup(
                           input.coordinates |> Address.Coordinates.accountIdx,
                           input.coordinates
                           |> Address.Coordinates.keyChainIdent,
                         );
                    let allCustodians =
                      keyChain.custodianKeyChains->(List.map(fst))
                      |> List.toArray
                      |> Set.mergeMany(UserId.emptySet);
                    let notSigned =
                      allCustodians->(Set.diff(custodiansThatSigned));
                    {
                      custodians:
                        notSigned->(Set.intersect(currentCustodians)),
                      sigs:
                        (
                          txWrapper.inputs->(Array.getExn(idx)).sequence
                          != Bitcoin.Transaction.defaultSequence ?
                            1 : keyChain.nCoSigners
                        )
                        - (Set.size(allCustodians) - Set.size(notSigned)),
                    };
                  })
                );
  let mandatory =
    missingSigs->(
                   Array.reduceU(
                     UserId.emptySet, (. required, {custodians, sigs}) =>
                     sigs > 0 && Set.size(custodians) == sigs ?
                       required->(Set.union(custodians)) : required
                   )
                 );
  {
    mandatory,
    additional:
      missingSigs->(
                     Array.reduceU(
                       UserId.emptySet, (. additional, {custodians, sigs}) =>
                       if (sigs > 0) {
                         let definetlySigned =
                           custodians->(Set.intersect(mandatory));
                         Set.size(definetlySigned) >= sigs ?
                           additional :
                           custodians
                           ->(Set.diff(definetlySigned))
                           ->(Set.union(additional));
                       } else {
                         additional;
                       }
                     )
                   ),
  };
};
