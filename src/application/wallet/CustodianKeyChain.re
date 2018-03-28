open PrimitiveTypes;

open WalletTypes;

open Bitcoin;

type t = {
  accountIndex: accountIdx,
  keyChainIndex: custodianKeyChainIdx,
  hdNode: HDNode.t
};

type public = t;

let accountIndex = public => public.accountIndex;

let keyChainIndex = public => public.keyChainIndex;

let hdNode = public => public.hdNode;

/* m/misthos'/venture'/coin_type'/account'/keyChain'/bip45'/cosignerIdx/change/address */
let misthosPurposeIndex = 1337;

let coinTypeBitcoin = 0;

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
    |> HDNode.deriveHardened(coinTypeBitcoin)
    |> HDNode.deriveHardened(accountIndex |> AccountIndex.toInt)
    |> HDNode.deriveHardened(keyChainIndex |> CustodianKeyChainIndex.toInt)
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
      ("accountIndex", AccountIndex.encode(keyChain.accountIndex)),
      ("keyChainIndex", CustodianKeyChainIndex.encode(keyChain.keyChainIndex)),
      ("hdNode", string(keyChain.hdNode |> Bitcoin.HDNode.toBase58))
    ])
  );

let decode = raw =>
  Json.Decode.{
    accountIndex: raw |> field("accountIndex", AccountIndex.decode),
    keyChainIndex: raw |> field("keyChainIndex", CustodianKeyChainIndex.decode),
    hdNode: raw |> field("hdNode", string) |> Bitcoin.HDNode.fromBase58
  };
