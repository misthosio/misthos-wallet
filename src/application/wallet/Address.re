open PrimitiveTypes;

open WalletTypes;

open Bitcoin;

module Coordinates = {
  type t = (
    AccountIndex.t,
    AccountKeyChain.Identifier.t,
    CoSignerIndex.t,
    ChainIndex.t,
    AddressIndex.t,
  );
  let next =
      (
        coSigner,
        usedCoordinates,
        chainIdx,
        {accountIdx, identifier, custodianKeyChains}: AccountKeyChain.t,
      ) => {
    let coSignerIdx =
      custodianKeyChains
      |> List.map(chain =>
           (chain |> fst, chain |> snd |> CustodianKeyChain.hdNode)
         )
      |> List.sort(((_, chainA), (_, chainB)) =>
           compare(
             chainA |> Bitcoin.HDNode.getPublicKey |> Utils.bufToHex,
             chainB |> Bitcoin.HDNode.getPublicKey |> Utils.bufToHex,
           )
         )
      |> List.mapi((i, (user, _)) =>
           UserId.eq(user, coSigner) ? Some(i) : None
         )
      |> List.find(Js.Option.isSome)
      |> Js.Option.getExn
      |> CoSignerIndex.fromInt;
    let addressIdx =
      usedCoordinates
      |> List.fold_left(
           (res, (aIdx, ident, coIdx, cIdx, addressIdx)) =>
             if (AccountIndex.eq(accountIdx, aIdx)
                 && AccountKeyChain.Identifier.eq(identifier, ident)
                 && CoSignerIndex.eq(coSignerIdx, coIdx)
                 && ChainIndex.eq(chainIdx, cIdx)) {
               AddressIndex.compare(addressIdx, res) > 0 ? addressIdx : res;
             } else {
               res;
             },
           (-1) |> AddressIndex.fromInt,
         );
    (
      accountIdx,
      identifier,
      coSignerIdx,
      chainIdx,
      addressIdx |> AddressIndex.next,
    );
  };
  let nextInternal = (user, usedCoordinates, accountKeyChain) =>
    next(user, usedCoordinates, ChainIndex.internalChain, accountKeyChain);
  let nextExternal = (user, usedCoordinates, accountKeyChain) =>
    next(user, usedCoordinates, ChainIndex.externalChain, accountKeyChain);
  let accountIdx = ((idx, _, _, _, _)) => idx;
  let keyChainIdent = ((_, ident, _, _, _)) => ident;
  let coSignerIdx = ((_, _, coSignerIdx, _, _)) => coSignerIdx;
  let chainIdx = ((_, _, _, chainIdx, _)) => chainIdx;
  let addressIdx = ((_, _, _, _, addressIdx)) => addressIdx;
  let allForAccount = aIdx =>
    List.filter(c => AccountIndex.eq(c |> accountIdx, aIdx));
  let encode = ((a, b, c, d, e)) =>
    Json.Encode.(
      ((a, b), (c, d, e))
      |> tuple2(
           tuple2(AccountIndex.encode, AccountKeyChain.Identifier.encode),
           tuple3(
             CoSignerIndex.encode,
             ChainIndex.encode,
             AddressIndex.encode,
           ),
         )
    );
  let decode = raw => {
    let ((a, b), (c, d, e)) =
      Json.Decode.(
        raw
        |> tuple2(
             tuple2(AccountIndex.decode, AccountKeyChain.Identifier.decode),
             tuple3(
               CoSignerIndex.decode,
               ChainIndex.decode,
               AddressIndex.decode,
             ),
           )
      );
    (a, b, c, d, e);
  };
};

type t = {
  nCoSigners: int,
  nPubKeys: int,
  coordinates: Coordinates.t,
  witnessScript: string,
  redeemScript: string,
  displayAddress: string,
  sequence: option(int),
};

let encode = address =>
  Json.Encode.(
    object_([
      ("nCoSigners", int(address.nCoSigners)),
      ("nPubKeys", int(address.nPubKeys)),
      ("coordinates", Coordinates.encode(address.coordinates)),
      ("witnessScript", string(address.witnessScript)),
      ("redeemScript", string(address.redeemScript)),
      ("displayAddress", string(address.displayAddress)),
      ("sequence", nullable(int, address.sequence)),
    ])
  );

let decode = raw =>
  Json.Decode.{
    nCoSigners: raw |> field("nCoSigners", int),
    nPubKeys: raw |> field("nPubKeys", int),
    coordinates: raw |> field("coordinates", Coordinates.decode),
    witnessScript: raw |> field("witnessScript", string),
    redeemScript: raw |> field("redeemScript", string),
    displayAddress: raw |> field("displayAddress", string),
    sequence: raw |> optional(field("sequence", int)),
  };

let make =
    (
      coordinates,
      {custodianKeyChains, nCoSigners, sequence}: AccountKeyChain.t,
    ) => {
  let pubKeys =
    custodianKeyChains
    |> List.map(chain => chain |> snd |> CustodianKeyChain.hdNode)
    |> List.map(node =>
         node
         ->(
             HDNode.derive(
               Coordinates.coSignerIdx(coordinates) |> CoSignerIndex.toInt,
             )
           )
         ->(
             HDNode.derive(
               Coordinates.chainIdx(coordinates) |> ChainIndex.toInt,
             )
           )
         ->(
             HDNode.derive(
               Coordinates.addressIdx(coordinates) |> AddressIndex.toInt,
             )
           )
       )
    |> List.map(node => node |> HDNode.getPublicKey)
    |> List.sort((pubKeyA, pubKeyB) =>
         compare(pubKeyA |> Utils.bufToHex, pubKeyB |> Utils.bufToHex)
       )
    |> Array.of_list;
  let network =
    custodianKeyChains
    |> List.hd
    |> snd
    |> CustodianKeyChain.hdNode
    |> HDNode.getNetwork;

  let witnessScript =
    switch (sequence) {
    | Some(sequence) =>
      MultisigWithSequence.encode(nCoSigners, pubKeys, sequence)
    | None =>
      Payments.multisig({
        "m": nCoSigners,
        "pubkeys": pubKeys,
        "network": network,
      })##output
    };
  let redeemScript =
    Payments.witnessScriptHash({
      "redeem": {
        "output": witnessScript,
      },
      "network": network,
    })##output;
  let displayAddress =
    Payments.scriptHash({
      "redeem": {
        "output": redeemScript,
      },
      "network": network,
    })##address;
  {
    nCoSigners,
    coordinates,
    nPubKeys: custodianKeyChains |> List.length,
    witnessScript: Utils.bufToHex(witnessScript),
    redeemScript: Utils.bufToHex(redeemScript),
    displayAddress,
    sequence,
  };
};

let find = (coordinates, keyChains) =>
  AccountKeyChain.Collection.lookup(
    coordinates |> Coordinates.accountIdx,
    coordinates |> Coordinates.keyChainIdent,
    keyChains,
  )
  |> make(coordinates);
