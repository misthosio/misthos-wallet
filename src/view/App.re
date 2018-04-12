open Session;

let component = ReasonReact.statelessComponent("App");

let make = (~session, ~updateSession, _children) => {
  ...component,
  render: _self => {
    let drawer = (currentRoute: Router.Config.route) =>
      switch (session, currentRoute) {
      | (NotLoggedIn | LoginPending | AnonymousLogin, _) => None
      | (Session.LoggedIn(data), Home) => Some(<VentureList session />)
      };
    let body = (currentRoute: Router.Config.route) =>
      switch (session, currentRoute) {
      | (NotLoggedIn | LoginPending | AnonymousLogin, _) =>
        <PublicHome onSignIn=(_e => updateSession(UserSession.SignIn)) />
      | (Session.LoggedIn(data), Home) => <Home data />
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
