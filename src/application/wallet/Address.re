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
             chainA |> Bitcoin.HDNode.getPublicKeyBuffer |> Utils.bufToHex,
             chainB |> Bitcoin.HDNode.getPublicKeyBuffer |> Utils.bufToHex,
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
    object_(
      Belt.List.concat(
        [
          ("nCoSigners", int(address.nCoSigners)),
          ("nPubKeys", int(address.nPubKeys)),
          ("coordinates", Coordinates.encode(address.coordinates)),
          ("witnessScript", string(address.witnessScript)),
          ("redeemScript", string(address.redeemScript)),
          ("displayAddress", string(address.displayAddress)),
        ],
        switch (address.sequence) {
        | None => []
        | Some(sequence) => [("sequence", int(sequence))]
        },
      ),
    )
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
  let keys =
    custodianKeyChains
    |> List.map(chain => chain |> snd |> CustodianKeyChain.hdNode)
    |> List.map(node =>
         node
         |> HDNode.derive(
              Coordinates.coSignerIdx(coordinates) |> CoSignerIndex.toInt,
            )
         |> HDNode.derive(
              Coordinates.chainIdx(coordinates) |> ChainIndex.toInt,
            )
         |> HDNode.derive(
              Coordinates.addressIdx(coordinates) |> AddressIndex.toInt,
            )
       )
    |> List.map(node => node##keyPair)
    |> List.sort((pairA, pairB) =>
         compare(
           pairA |> ECPair.getPublicKeyBuffer |> Utils.bufToHex,
           pairB |> ECPair.getPublicKeyBuffer |> Utils.bufToHex,
         )
       );
  open Script;
  let witnessScript =
    sequence |> Js.Option.isSome ?
      MultisigWithSequence.encode(
        nCoSigners,
        keys |> List.map(ECPair.getPublicKeyBuffer) |> Array.of_list,
        sequence |> Js.Option.getExn,
      ) :
      Multisig.Output.encode(
        nCoSigners,
        keys |> List.map(ECPair.getPublicKeyBuffer) |> Array.of_list,
      );
  let redeemScript =
    WitnessScriptHash.Output.encode(Crypto.sha256FromBuffer(witnessScript));
  let outputScript = ScriptHash.Output.encode(Crypto.hash160(redeemScript));
  let displayAddress =
    Address.fromOutputScript(
      outputScript,
      keys |> List.hd |> ECPair.getNetwork,
    );
  {
    nCoSigners,
    coordinates,
    nPubKeys: custodianKeyChains |> List.length,
    witnessScript: Utils.bufToHex(witnessScript),
    redeemScript: Utils.bufToHex(redeemScript),
    displayAddress,
    sequence: None,
  };
};

let find = (coordinates, keyChains) =>
  AccountKeyChain.Collection.lookup(
    coordinates |> Coordinates.accountIdx,
    coordinates |> Coordinates.keyChainIdent,
    keyChains,
  )
  |> make(coordinates);
