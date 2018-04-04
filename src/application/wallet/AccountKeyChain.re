open PrimitiveTypes;

open WalletTypes;

open Bitcoin;

type t = {
  accountIdx,
  keyChainIdx: accountKeyChainIdx,
  nCoSigners: int,
  custodianKeyChains: list((userId, CustodianKeyChain.public))
};

let make = (accountIdx, keyChainIdx, nCoSigners, custodianKeyChains) => {
  accountIdx,
  keyChainIdx,
  custodianKeyChains,
  nCoSigners
};

module Address = {
  module Coordinates = {
    type t = (
      AccountIndex.t,
      AccountKeyChainIndex.t,
      ChainIndex.t,
      AddressIndex.t
    );
    let firstExternal = ({accountIdx, keyChainIdx}) => (
      accountIdx,
      keyChainIdx,
      ChainIndex.externalChain,
      AddressIndex.first
    );
    let firstInternal = ({accountIdx, keyChainIdx}) => (
      accountIdx,
      keyChainIdx,
      ChainIndex.internalChain,
      AddressIndex.first
    );
    let next = ((accountIdx, accountKeyChainIdx, chainIdx, addressIdx)) => (
      accountIdx,
      accountKeyChainIdx,
      chainIdx,
      addressIdx |> AddressIndex.next
    );
    let addressIdx = ((_, _, _, addressIdx)) => addressIdx;
    let keyChainIdx = ((_, keyChainIdx, _, _)) => keyChainIdx;
    let chainIdx = ((_, _, chainIdx, _)) => chainIdx;
    let accountIdx = ((idx, _, _, _)) => idx;
    let encode =
      Json.Encode.(
        tuple4(
          AccountIndex.encode,
          AccountKeyChainIndex.encode,
          ChainIndex.encode,
          AddressIndex.encode
        )
      );
    let decode =
      Json.Decode.(
        tuple4(
          AccountIndex.decode,
          AccountKeyChainIndex.decode,
          ChainIndex.decode,
          AddressIndex.decode
        )
      );
  };
  type t = {
    nCoSigners: int,
    coordinates: Coordinates.t,
    witnessScript: string,
    redeemScript: string,
    address: string
  };
  /* bip45'/cosignerIdx/change/address */
  let make = (coordinates, {custodianKeyChains, nCoSigners}) => {
    let keys =
      custodianKeyChains
      |> List.map(chain => chain |> snd |> CustodianKeyChain.hdNode)
      |> List.sort((chainA, chainB) =>
           compare(
             chainA |> Bitcoin.HDNode.getPublicKeyBuffer |> Utils.bufToHex,
             chainB |> Bitcoin.HDNode.getPublicKeyBuffer |> Utils.bufToHex
           )
         )
      |> List.map(node =>
           node
           |> HDNode.derive(CustodianKeyChain.defaultCosignerIdx)
           |> HDNode.derive(
                Coordinates.chainIdx(coordinates) |> ChainIndex.toInt
              )
           |> HDNode.derive(
                Coordinates.addressIdx(coordinates) |> AddressIndex.toInt
              )
         )
      |> List.map(node => node##keyPair);
    open Script;
    let witnessScript =
      Multisig.Output.encode(
        nCoSigners,
        keys |> List.map(ECPair.getPublicKeyBuffer) |> Array.of_list
      );
    let redeemScript =
      WitnessScriptHash.Output.encode(Crypto.sha256FromBuffer(witnessScript));
    let outputScript = ScriptHash.Output.encode(Crypto.hash160(redeemScript));
    let address =
      Address.fromOutputScript(
        outputScript,
        keys |> List.hd |> ECPair.getNetwork
      );
    {
      nCoSigners,
      coordinates,
      witnessScript: Utils.bufToHex(witnessScript),
      redeemScript: Utils.bufToHex(redeemScript),
      address
    };
  };
};

let custodianKeyChains = keyChain => keyChain.custodianKeyChains;

let lookupKeyChain =
    (
      (accountIdx, accountKeyChainIdx, _chainIdx, _addressIdx),
      accounts: list((accountIdx, list((accountKeyChainIdx, t))))
    ) =>
  accounts |> List.assoc(accountIdx) |> List.assoc(accountKeyChainIdx);

let find = (coordinates, keyChains) =>
  keyChains |> lookupKeyChain(coordinates) |> Address.make(coordinates);

let encode = keyChain =>
  Json.Encode.(
    object_([
      (
        "custodianKeyChains",
        list(
          pair(UserId.encode, CustodianKeyChain.encode),
          keyChain.custodianKeyChains
        )
      ),
      ("nCoSigners", int(keyChain.nCoSigners)),
      ("accountIdx", AccountIndex.encode(keyChain.accountIdx)),
      ("keyChainIdx", AccountKeyChainIndex.encode(keyChain.keyChainIdx))
    ])
  );

let decode = raw =>
  Json.Decode.{
    custodianKeyChains:
      raw
      |> field(
           "custodianKeyChains",
           list(pair(UserId.decode, CustodianKeyChain.decode))
         ),
    nCoSigners: raw |> field("nCoSigners", int),
    accountIdx: raw |> field("accountIdx", AccountIndex.decode),
    keyChainIdx: raw |> field("keyChainIdx", AccountKeyChainIndex.decode)
  };
