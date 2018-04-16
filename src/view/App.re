open Session;

let component = ReasonReact.statelessComponent("App");

let make = (~session, ~updateSession, _children) => {
  ...component,
  render: _self => {
    let onSignIn = _e => updateSession(SessionStore.SignIn);
    let onSignOut = _e => updateSession(SessionStore.SignOut);
    let drawer = (currentRoute: Router.Config.route) =>
      switch (session, currentRoute) {
      | (NotLoggedIn | LoginPending | AnonymousLogin | Unknown, _) => None
      | (LoggedIn(_data), Home) => Some(<Drawer onSignOut />)
      | (LoggedIn(_data), Venture(selected)) =>
        Some(<Drawer onSignOut selected />)
      };
    let body = (currentRoute: Router.Config.route) =>
      switch (session, currentRoute) {
      | (NotLoggedIn | LoginPending | AnonymousLogin | Unknown, _) =>
        <PublicHome onSignIn />
      | (LoggedIn(session), Home) => <Home session />
      | (LoggedIn(session), _) => <Home session />
      };
    <Router.Container>
      ...(
           (~currentRoute) =>
             <Layout drawer=(currentRoute |> drawer)>
               (currentRoute |> body)
             </Layout>
         )
    </Router.Container>;
  },
};
