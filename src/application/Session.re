open PrimitiveTypes;

module Data = {
  type t = {
    userId,
    appPrivateKey: string,
    issuerKeyPair: Bitcoin.ECPair.t,
    storagePrefix: string,
    masterKeyChain: Bitcoin.HDNode.t,
    network: Network.t,
  };
  let fromUserData = userData =>
    switch (Js.Nullable.toOption(userData##username)) {
    | None => None
    | Some(blockstackId) =>
      let issuerKeyPair =
        Utils.keyPairFromPrivateKey(
          Network.bitcoinNetwork(Testnet),
          userData##appPrivateKey,
        );
      Some({
        appPrivateKey: userData##appPrivateKey,
        userId: blockstackId |> UserId.fromString,
        issuerKeyPair,
        network: Testnet,
        storagePrefix:
          UserInfo.storagePrefix(
            ~appPubKey=issuerKeyPair |> Utils.publicKeyFromKeyPair,
          ),
        masterKeyChain:
          Bitcoin.HDNode.make(
            issuerKeyPair,
            Utils.bufFromHex(
              "c8bce5e6dac6f931af17863878cce2ca3b704c61b3d775fe56881cc8ff3ab1cb",
            ),
          ),
      });
    };
};

type t =
  | Unknown
  | LoginPending
  | NotLoggedIn
  | AnonymousLogin
  | LoggedIn(Data.t);

let initMasterKey = (sessionData: Data.t) => {
  let appPubKey = sessionData.issuerKeyPair |> Utils.publicKeyFromKeyPair;
  Js.Promise.(
    UserInfo.getOrInit(~appPubKey)
    |> then_(({chainCode}: UserInfo.Private.t) =>
         {
           ...sessionData,
           masterKeyChain:
             Bitcoin.HDNode.make(sessionData.issuerKeyPair, chainCode),
         }
         |> resolve
       )
  );
};

let completeLogIn = () =>
  Js.Promise.(
    Blockstack.handlePendingSignIn()
    |> then_(userData =>
         switch (Data.fromUserData(userData)) {
         | None => resolve(AnonymousLogin)
         | Some(sessionData) =>
           initMasterKey(sessionData)
           |> then_(session => LoggedIn(session) |> resolve)
         }
       )
  );

let getCurrentSession = () =>
  Js.Promise.(
    if (Blockstack.isUserSignedIn()) {
      switch (Blockstack.loadUserData()) {
      | None => NotLoggedIn |> resolve
      | Some(userData) =>
        switch (Data.fromUserData(userData)) {
        | None => AnonymousLogin |> resolve
        | Some(sessionData) =>
          initMasterKey(sessionData)
          |> then_(session => LoggedIn(session) |> resolve)
        }
      };
    } else if (Blockstack.isSignInPending()) {
      completeLogIn();
    } else {
      NotLoggedIn |> resolve;
    }
  );

let signIn = () => {
  Blockstack.redirectToSignIn(
    ~redirectURI=Location.origin ++ "/",
    ~manifestURI=Location.origin ++ "/manifest.json",
    ~scopes=[|"store_write", "publish_data"|],
  );
  LoginPending;
};

let signOut = () => {
  Blockstack.signUserOut();
  NotLoggedIn;
};
