open PrimitiveTypes;

type t = {
  userId,
  appPrivateKey: string,
  issuerKeyPair: Bitcoin.ECPair.t,
  storagePrefix: string,
  masterKeyChain: Bitcoin.HDNode.t,
  network: Network.t,
};

let fromUserData = (userData, network) =>
  switch (Js.Nullable.toOption(userData##username)) {
  | None => None
  | Some(blockstackId) =>
    let issuerKeyPair =
      Utils.keyPairFromPrivateKey(
        Network.bitcoinNetwork(network),
        userData##appPrivateKey,
      );
    Some({
      appPrivateKey: userData##appPrivateKey,
      userId: blockstackId |> UserId.fromString,
      issuerKeyPair,
      network,
      storagePrefix:
        UserInfo.storagePrefix(
          ~appPubKey=issuerKeyPair |> Utils.publicKeyFromKeyPair,
        ),
      masterKeyChain:
        Bitcoin.(
          HDNode.fromPrivateKey(
            issuerKeyPair |> ECPair.getPrivateKey,
            Utils.bufFromHex(
              "c8bce5e6dac6f931af17863878cce2ca3b704c61b3d775fe56881cc8ff3ab1cb",
            ),
            issuerKeyPair |> ECPair.getNetwork,
          )
        ),
    });
  };
