open PrimitiveTypes;

open Bitcoin;

type t = {
  accountIndex: int,
  keyChainIndex: int,
  hdNode: HDNode.t
};

type public = t;

let getAccountIndex = public => public.accountIndex;

let getKeyChainIndex = public => public.keyChainIndex;

/* m/misthos'/venture'/currency'/account'/keyChain'/bip45'/cosignerIdx/change/address */
let misthosPurposeIndex = 1337;

let bip45Purpose = 45;

let make = (~ventureId, ~accountIndex, ~keyChainIndex, ~masterKeyChain) => {
  let misthosKeyChain =
    masterKeyChain |> HDNode.deriveHardened(misthosPurposeIndex);
  let salt =
    misthosKeyChain
    |> HDNode.getPublicKeyBuffer
    |> Utils.bufToHex
    |> Utils.hash;
  let custodianKeyChain =
    misthosKeyChain
    |> HDNode.deriveHardened(
         Utils.hash(VentureId.toString(ventureId) ++ salt) |> Utils.hashCode
       )
    |> HDNode.deriveHardened(accountIndex)
    |> HDNode.deriveHardened(keyChainIndex)
    |> HDNode.deriveHardened(bip45Purpose);
  {accountIndex, keyChainIndex, hdNode: custodianKeyChain};
};

let toPublicKeyChain = keyChain => {
  ...keyChain,
  hdNode: keyChain.hdNode |> HDNode.neutered
};

let encode = keyChain =>
  Json.Encode.(
    object_([
      ("accountIndex", int(keyChain.accountIndex)),
      ("keyChainIndex", int(keyChain.keyChainIndex)),
      ("hdNode", string(keyChain.hdNode |> Bitcoin.HDNode.toBase58))
    ])
  );

let decode = raw =>
  Json.Decode.{
    accountIndex: raw |> field("accountIndex", int),
    keyChainIndex: raw |> field("keyChainIndex", int),
    hdNode: raw |> field("hdNode", string) |> Bitcoin.HDNode.fromBase58
  };
