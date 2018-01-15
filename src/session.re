type data = {
  blockstackId: string,
  appKeyPair: Bitcoin.ECPair.t
};

type t =
  | NotLoggedIn
  | LoginPending
  | AnonymousLogin
  | LoggedIn(data);

let sessionDataFromUserData = userData =>
  switch (Js.Nullable.to_opt(userData##username)) {
  | None => None
  | Some(blockstackId) =>
    Some({
      blockstackId,
      appKeyPair: userData##appPrivateKey |> Utils.keyPairFromPrivateKey
    })
  };

let getCurrentSession = () =>
  if (Blockstack.isUserSignedIn()) {
    switch (Blockstack.loadUserData()) {
    | None => NotLoggedIn
    | Some(userData) =>
      switch (sessionDataFromUserData(userData)) {
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
         switch (sessionDataFromUserData(userData)) {
         | None => resolve(AnonymousLogin)
         | Some(sessionData) =>
           let appPubKey =
             sessionData.appKeyPair |> Utils.publicKeyFromKeyPair;
           Public.writePublicFile(~appPubKey)
           |> then_(() => resolve(LoggedIn(sessionData)));
         }
       )
  );

let signIn = () => {
  Blockstack.redirectToSignIn();
  LoginPending;
};

let signOut = () => {
  Blockstack.signUserOut();
  NotLoggedIn;
};
