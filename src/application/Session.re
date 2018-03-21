open PrimitiveTypes;

module Data = {
  type t = {
    userId,
    appKeyPair: Bitcoin.ECPair.t,
    address: string,
    chainCode: Node.buffer
  };
  let fromUserData = userData =>
    switch (Js.Nullable.toOption(userData##username)) {
    | None => None
    | Some(blockstackId) =>
      let appKeyPair = userData##appPrivateKey |> Utils.keyPairFromPrivateKey;
      Some({
        userId: blockstackId |> UserId.fromString,
        appKeyPair,
        address: appKeyPair |> Bitcoin.ECPair.getAddress,
        chainCode: Node_buffer.fromString("")
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
                resolve(LoggedIn({...sessionData, chainCode}))
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
