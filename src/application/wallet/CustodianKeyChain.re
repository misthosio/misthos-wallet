open PrimitiveTypes;

open WalletTypes;

open Bitcoin;

type t = {
  accountIdx,
  keyChainIdx: custodianKeyChainIdx,
  hdNode: HDNode.t,
};

type public = t;

let accountIdx = public => public.accountIdx;

let keyChainIdx = public => public.keyChainIdx;

let hdNode = public => public.hdNode;

/* m/misthosPurpose' /venture'/coin_type'/account'/keyChain'/bip45'/cosignerIdx/change/address */
let misthosWalletPurposeIdx = 0;

let coinTypeBitcoin = 0;

let bip45Purpose = 45;

let make = (~ventureId, ~accountIdx, ~keyChainIdx, ~masterKeyChain) => {
  let misthosKeyChain =
    masterKeyChain |> HDNode.deriveHardened(misthosWalletPurposeIdx);
  let salt =
    misthosKeyChain
    |> HDNode.getPublicKeyBuffer
    |> Utils.bufToHex
    |> Utils.hash;
  let custodianKeyChain =
    misthosKeyChain
    |> HDNode.deriveHardened(
         Utils.hash(VentureId.toString(ventureId) ++ salt) |> Utils.hashCode,
       )
    |> HDNode.deriveHardened(coinTypeBitcoin)
    |> HDNode.deriveHardened(accountIdx |> AccountIndex.toInt)
    |> HDNode.deriveHardened(keyChainIdx |> CustodianKeyChainIndex.toInt)
    |> HDNode.deriveHardened(bip45Purpose);
  {accountIdx, keyChainIdx, hdNode: custodianKeyChain};
};

let toPublicKeyChain = keyChain => {
  ...keyChain,
  hdNode: keyChain.hdNode |> HDNode.neutered,
};

let defaultCosignerIdx = 0;

let getSigningKey = (chainIdx, addressIdx, keyChain) => (
                                                           keyChain.hdNode
                                                           |> HDNode.derive(
                                                                defaultCosignerIdx,
                                                              )
                                                           |> HDNode.derive(
                                                                chainIdx
                                                                |> ChainIndex.toInt,
                                                              )
                                                           |> HDNode.derive(
                                                                addressIdx
                                                                |> AddressIndex.toInt,
                                                              )
                                                         )##keyPair;

let encode = keyChain =>
  Json.Encode.(
    object_([
      ("accountIndex", AccountIndex.encode(keyChain.accountIdx)),
      ("keyChainIndex", CustodianKeyChainIndex.encode(keyChain.keyChainIdx)),
      ("hdNode", string(keyChain.hdNode |> Bitcoin.HDNode.toBase58)),
    ])
  );

let decode = raw =>
  Json.Decode.{
    accountIdx: raw |> field("accountIndex", AccountIndex.decode),
    keyChainIdx: raw |> field("keyChainIndex", CustodianKeyChainIndex.decode),
    hdNode: raw |> field("hdNode", string) |> Bitcoin.HDNode.fromBase58,
  };
