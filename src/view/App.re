open Session;

let component = ReasonReact.statelessComponent("App");

let make = (~session, ~updateSession, _children) => {
  ...component,
  render: _self => {
    let drawer = (currentRoute: Router.Config.route) =>
      switch (session, currentRoute) {
      | (NotLoggedIn | LoginPending | AnonymousLogin, _) => None
      | (Session.LoggedIn(data), Home) => Some(<VentureList session />)
      | (Session.LoggedIn(data), Venture(selected)) =>
        Some(<VentureList selected session />)
      };
    let body = (currentRoute: Router.Config.route) =>
      switch (session, currentRoute) {
      | (NotLoggedIn | LoginPending | AnonymousLogin, _) =>
        <PublicHome onSignIn=(_e => updateSession(SessionStore.SignIn)) />
      | (Session.LoggedIn(session), Home) => <Home session />
      | (Session.LoggedIn(session), _) => <Home session />
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
