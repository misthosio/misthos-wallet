open Session;

let component = ReasonReact.statelessComponent("App");

let make = (~session, ~updateSession, _children) => {
  ...component,
  render: _self => {
    let drawer = (currentRoute: Router.Config.route) =>
      switch (session, currentRoute) {
      | (NotLoggedIn | LoginPending | AnonymousLogin | Unknown, _) => None
      | (LoggedIn(_data), Home) => Some(<VentureList session />)
      | (LoggedIn(_data), Venture(selected)) =>
        Some(<VentureList selected session />)
      };
    let body = (currentRoute: Router.Config.route) =>
      switch (session, currentRoute) {
      | (NotLoggedIn | LoginPending | AnonymousLogin | Unknown, _) =>
        <PublicHome onSignIn=(_e => updateSession(SessionStore.SignIn)) />
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
