open PrimitiveTypes;

module Data = {
  type t = {
    userId,
    appKeyPair: Bitcoin.ECPair.t,
    storagePrefix: string,
    masterKeyChain: Bitcoin.HDNode.t,
    network: Network.t
  };
  let fromUserData = userData =>
    switch (Js.Nullable.toOption(userData##username)) {
    | None => None
    | Some(blockstackId) =>
      let appKeyPair =
        Utils.keyPairFromPrivateKey(
          Network.Regtest.network,
          userData##appPrivateKey
        );
      let appPubKey = appKeyPair |> Utils.publicKeyFromKeyPair;
      Some({
        userId: blockstackId |> UserId.fromString,
        appKeyPair,
        network: Regtest,
        storagePrefix: UserInfo.storagePrefix(~appPubKey),
        masterKeyChain:
          Bitcoin.HDNode.make(
            appKeyPair,
            Utils.bufFromHex(
              "c8bce5e6dac6f931af17863878cce2ca3b704c61b3d775fe56881cc8ff3ab1cb"
            )
          )
      });
    };
};

type t =
  | NotLoggedIn
  | LoginPending
  | AnonymousLogin
  | LoggedIn(Data.t);

let getCurrentSession = () =>
  if (Blockstack.isUserSignedIn()) {
    switch (Blockstack.loadUserData()) {
    | None => NotLoggedIn
    | Some(userData) =>
      switch (Data.fromUserData(userData)) {
      | None => AnonymousLogin
      | Some(sessionData) => LoggedIn(sessionData)
      }
    };
  } else if (Blockstack.isSignInPending()) {
    LoginPending;
  } else {
    NotLoggedIn;
  };

let completeLogIn = () =>
  Js.Promise.(
    Blockstack.handlePendingSignIn()
    |> then_(userData =>
         switch (Data.fromUserData(userData)) {
         | None => resolve(AnonymousLogin)
         | Some(sessionData) =>
           let appPubKey =
             sessionData.appKeyPair |> Utils.publicKeyFromKeyPair;
           UserInfo.getOrInit(~appPubKey)
           |> then_(({chainCode}: UserInfo.Private.t) =>
                resolve(
                  LoggedIn({
                    ...sessionData,
                    masterKeyChain:
                      Bitcoin.HDNode.make(sessionData.appKeyPair, chainCode)
                  })
                )
              );
         }
       )
  );

let signIn = () => {
  Blockstack.redirectToSignIn(
    ~redirectURI=Location.origin ++ "/",
    ~manifestURI=Location.origin ++ "/manifest.json",
    ~scopes=[|"store_write", "publish_data"|]
  );
  LoginPending;
};

let signOut = () => {
  Blockstack.signUserOut();
  NotLoggedIn;
};
