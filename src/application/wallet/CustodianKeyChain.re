open PrimitiveTypes;

open WalletTypes;

open Bitcoin;

type t = {
  hardwareId: option(string),
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
let misthosWalletPurposePath = "0'";

let coinTypeBitcoin = 0;

let bip45Purpose = 45;

let makePathToBip45Root =
    (~ventureId, ~accountIdx, ~keyChainIdx, misthosPurposeNode) => {
  let salt =
    misthosPurposeNode |> HDNode.getPublicKey |> Utils.bufToHex |> Utils.hash;
  let ventureIdx =
    Utils.hash(VentureId.toString(ventureId) ++ salt) |> Utils.hashCode;
  "0'/"
  ++ string_of_int(ventureIdx)
  ++ "'/"
  ++ string_of_int(coinTypeBitcoin)
  ++ "'/"
  ++ string_of_int(accountIdx |> AccountIndex.toInt)
  ++ "'/"
  ++ string_of_int(keyChainIdx |> CustodianKeyChainIndex.toInt)
  ++ "'/"
  ++ string_of_int(bip45Purpose)
  ++ "'";
};

let fromHardwareNode = (~hardwareId, ~accountIdx, ~keyChainIdx, hdNode) => {
  hardwareId: Some(hardwareId),
  accountIdx,
  keyChainIdx,
  hdNode: hdNode |> HDNode.neutered,
};

let make = (~ventureId, ~accountIdx, ~keyChainIdx, ~masterKeyChain) => {
  let misthosPurposeNode =
    masterKeyChain |> HDNode.deriveHardened(misthosWalletPurposeIdx);
  let custodianKeyChain =
    masterKeyChain
    |> HDNode.derivePath(
         makePathToBip45Root(
           ~ventureId,
           ~accountIdx,
           ~keyChainIdx,
           misthosPurposeNode,
         ),
       );
  /* let custodianKeyChain = */
  /*   misthosKeyChain */
  /*   |> HDNode.deriveHardened( */
  /*        Utils.hash(VentureId.toString(ventureId) ++ salt) |> Utils.hashCode, */
  /*      ) */
  /*   |> HDNode.deriveHardened(coinTypeBitcoin) */
  /*   |> HDNode.deriveHardened(accountIdx |> AccountIndex.toInt) */
  /*   |> HDNode.deriveHardened(keyChainIdx |> CustodianKeyChainIndex.toInt) */
  /*   |> HDNode.deriveHardened(bip45Purpose); */
  {hardwareId: None, accountIdx, keyChainIdx, hdNode: custodianKeyChain};
};

let toPublicKeyChain = keyChain => {
  ...keyChain,
  hdNode: keyChain.hdNode |> HDNode.neutered,
};

let getSigningKey = (coSignerIdx, chainIdx, addressIdx, keyChain) =>
  keyChain.hdNode
  |> HDNode.derive(coSignerIdx |> CoSignerIndex.toInt)
  |> HDNode.derive(chainIdx |> ChainIndex.toInt)
  |> HDNode.derive(addressIdx |> AddressIndex.toInt)
  |> HDNode.getPrivateKey
  |. ECPair.fromPrivateKey({"network": keyChain.hdNode |. HDNode.getNetwork});

let encode = keyChain =>
  Json.Encode.(
    object_([
      ("hardwareId", nullable(string, keyChain.hardwareId)),
      ("accountIndex", AccountIndex.encode(keyChain.accountIdx)),
      ("keyChainIndex", CustodianKeyChainIndex.encode(keyChain.keyChainIdx)),
      ("hdNode", string(keyChain.hdNode |> Bitcoin.HDNode.toBase58)),
    ])
  );

let decode = raw =>
  Json.Decode.{
    hardwareId: raw |> Utils.maybeField("hardwareId", string),
    accountIdx: raw |> field("accountIndex", AccountIndex.decode),
    keyChainIdx: raw |> field("keyChainIndex", CustodianKeyChainIndex.decode),
    hdNode: raw |> field("hdNode", string) |> Bitcoin.HDNode.fromBase58,
  };

let eq = (a, b) => encode(a) == encode(b);
