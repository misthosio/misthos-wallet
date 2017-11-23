type user = {userName: string};

type session =
  | NotLoggedIn
  | LoggedIn(user);

let getCurrentSession = () =>
  if (Blockstack.isUserSignedIn()) {
    switch (Blockstack.loadUserData()) {
    | None => NotLoggedIn
    | Some(userData) =>
      let userName =
        switch (Js.Nullable.to_opt(userData##username)) {
        | None => "Anonymous"
        | Some(name) => name
        };
      LoggedIn({userName: userName})
    }
  } else {
    if (Blockstack.isSignInPending()) {
      Blockstack.handlePendingSignIn()
    };
    NotLoggedIn
  };

let signIn = () => Blockstack.redirectToSignIn();

let signOut = () => Blockstack.signUserOut();
