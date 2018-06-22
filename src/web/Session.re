type t =
  | Unknown
  | LoginPending
  | NotLoggedIn
  | NamelessLogin
  | LoggedIn(SessionData.t);

let initMasterKey = (sessionData: SessionData.t) => {
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

let completeLogIn = () => {
  let environment = Environment.get();
  Cookie.get("transitKey")
  |> Utils.mapOption(key => {
       LocalStorage.setItem("blockstack-transit-private-key", key);
       Cookie.delete("transitKey", environment.cookieDomain);
     })
  |> ignore;
  Js.Promise.(
    Blockstack.handlePendingSignIn()
    |> then_(userData =>
         switch (SessionData.fromUserData(userData, environment.network)) {
         | None => resolve(NamelessLogin)
         | Some(sessionData) =>
           initMasterKey(sessionData)
           |> then_(session => LoggedIn(session) |> resolve)
         }
       )
  );
};

let getCurrentSession = () =>
  Js.Promise.(
    if (Blockstack.isUserSignedIn()) {
      switch (Blockstack.loadUserData()) {
      | None => NotLoggedIn |> resolve
      | Some(userData) =>
        switch (SessionData.fromUserData(userData, Environment.get().network)) {
        | None => NamelessLogin |> resolve
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

let signOut = () => {
  Blockstack.signUserOut();
  Location.replace(Environment.get().webDomain);
  NotLoggedIn;
};

let signIn = () => {
  signOut() |> ignore;
  let transitKey = Blockstack.makeECPrivateKey();
  let environment = Environment.get();
  Cookie.set("transitKey", transitKey, environment.cookieDomain);

  Blockstack.(
    redirectToSignInWithAuthRequest(
      makeAuthRequest(
        ~transitKey,
        ~redirectURI=environment.redirectURI,
        ~manifestURI=environment.manifestURI,
        ~scopes=[|"store_write", "publish_data"|],
        ~appDomain=environment.appDomain,
      ),
    )
  );
  LoginPending;
};
