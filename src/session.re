module Data = {
  type t = {
    blockstackId: string,
    appKeyPair: Bitcoin.ECPair.t
  };
  let fromUserData = userData =>
    switch (Js.Nullable.to_opt(userData##username)) {
    | None => None
    | Some(blockstackId) =>
      Some({
        blockstackId,
        appKeyPair: userData##appPrivateKey |> Utils.keyPairFromPrivateKey
      })
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
           UserPublicData.persistPublicData(~appPubKey)
           |> then_(() => resolve(LoggedIn(sessionData)));
         }
       )
  );

let signIn = () => {
  Blockstack.redirectToSignIn(
    ~redirectURI=[%bs.raw {|window.location.origin|}] ++ "/",
    ~manifestURI=[%bs.raw {|window.location.origin|}] ++ "/manifest.json",
    ~scopes=[|"store_write", "publish_data"|]
  );
  LoginPending;
};

let signOut = () => {
  Blockstack.signUserOut();
  NotLoggedIn;
};
