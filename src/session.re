type data = {userName: string};

type t =
  | NotLoggedIn
  | LoginPending
  | LoggedIn(data);

let sessionFromUserData = (userData) => {
  let userName =
    switch (Js.Nullable.to_opt(userData##username)) {
    | None => "Anonymous"
    | Some(name) => name
    };
  {userName: userName}
};

let getCurrentSession = () =>
  if (Blockstack.isUserSignedIn()) {
    switch (Blockstack.loadUserData()) {
    | None => NotLoggedIn
    | Some(userData) => LoggedIn(sessionFromUserData(userData))
    }
  } else if (Blockstack.isSignInPending()) {
    LoginPending
  } else {
    NotLoggedIn
  };

let completeLogIn = () =>
  Js.Promise.(
    Blockstack.handlePendingSignIn()
    |> then_((userData) => LoggedIn(sessionFromUserData(userData)) |> resolve)
  );

let signIn = () => {
  Blockstack.redirectToSignIn();
  LoginPending
};

let signOut = () => {
  Blockstack.signUserOut();
  NotLoggedIn
};
