open PrimitiveTypes;

open WalletTypes;

open Bitcoin;

module Coordinates = {
  type t = (
    AccountIndex.t,
    AccountKeyChainIndex.t,
    CoSignerIndex.t,
    ChainIndex.t,
    AddressIndex.t,
  );
  let next =
      (
        coSigner,
        usedCoordinates,
        chainIdx,
        {accountIdx, keyChainIdx, custodianKeyChains}: AccountKeyChain.t,
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
           (res, (aIdx, kIdx, coIdx, cIdx, addressIdx)) =>
             if (AccountIndex.eq(accountIdx, aIdx)
                 && AccountKeyChainIndex.eq(keyChainIdx, kIdx)
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
      keyChainIdx,
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
  let keyChainIdx = ((_, keyChainIdx, _, _, _)) => keyChainIdx;
  let coSignerIdx = ((_, _, coSignerIdx, _, _)) => coSignerIdx;
  let chainIdx = ((_, _, _, chainIdx, _)) => chainIdx;
  let addressIdx = ((_, _, _, _, addressIdx)) => addressIdx;
  let encode = ((a, b, c, d, e)) =>
    Json.Encode.(
      ((a, b), (c, d, e))
      |> tuple2(
           tuple2(AccountIndex.encode, AccountKeyChainIndex.encode),
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
             tuple2(AccountIndex.decode, AccountKeyChainIndex.decode),
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
  address: string,
};

let make = (coordinates, {custodianKeyChains, nCoSigners}: AccountKeyChain.t) => {
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
    Multisig.Output.encode(
      nCoSigners,
      keys |> List.map(ECPair.getPublicKeyBuffer) |> Array.of_list,
    );
  let redeemScript =
    WitnessScriptHash.Output.encode(Crypto.sha256FromBuffer(witnessScript));
  let outputScript = ScriptHash.Output.encode(Crypto.hash160(redeemScript));
  let address =
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
    address,
  };
};
