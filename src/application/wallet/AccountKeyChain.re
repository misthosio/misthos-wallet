open PrimitiveTypes;

open WalletTypes;

open Bitcoin;

type t = {
  nCoSigners: int,
  custodianKeyChains: list((userId, CustodianKeyChain.public))
};

let make = (nCoSigners, custodianKeyChains) => {
  custodianKeyChains,
  nCoSigners
};

module Address = {
  type t = {
    nCoSigners: int,
    addressIdx,
    chain: int,
    witnessScript: string,
    redeemScript: string,
    address: string
  };
  /* bip45'/cosignerIdx/change/address */
  let make = (chain, index, {custodianKeyChains, nCoSigners}) => {
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
           |> HDNode.derive(chain)
           |> HDNode.derive(index |> AddressIndex.toInt)
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
      addressIdx: index,
      chain,
      witnessScript: Utils.bufToHex(witnessScript),
      redeemScript: Utils.bufToHex(redeemScript),
      address
    };
  };
};

let custodianKeyChains = keyChain => keyChain.custodianKeyChains;

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
      ("nCoSigners", int(keyChain.nCoSigners))
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
    nCoSigners: raw |> field("nCoSigners", int)
  };

let find = (coordinates, keyChains) =>
  keyChains
  |> AddressCoordinates.lookupKeyChain(coordinates)
  |> Address.make(
       coordinates |> AddressCoordinates.chainIdx,
       coordinates |> AddressCoordinates.addressIdx
     );
