open Session;

let component = ReasonReact.statelessComponent("App");

let make = (~session, ~updateSession, _children) => {
  ...component,
  render: _self => {
    let onSignIn = _e => updateSession(SessionStore.SignIn);
    let onSignOut = _e => updateSession(SessionStore.SignOut);
    let selectedVentureId = (route: Router.Config.route) =>
      switch (route) {
      | Venture(selected) => Some(selected)
      | _ => None
      };
    let drawer = (index, currentRoute: Router.Config.route) =>
      switch (session, currentRoute) {
      | (NotLoggedIn | LoginPending | AnonymousLogin | Unknown, _) => None
      | (LoggedIn(_data), Home) => Some(<Drawer onSignOut index />)
      | (LoggedIn(_data), Venture(selected)) =>
        Some(<Drawer onSignOut selected index />)
      };
    let body = (selectedVenture, currentRoute: Router.Config.route) =>
      switch (session, currentRoute) {
      | (NotLoggedIn | LoginPending | AnonymousLogin | Unknown, _) =>
        <PublicHome onSignIn />
      | (LoggedIn(session), Home) => <Home session ?selectedVenture />
      | (LoggedIn(session), _) => <Home session ?selectedVenture />
      };
    <Router.Container>
      ...(
           (~currentRoute) =>
             <VentureStore
               selectedVentureId=?(selectedVentureId(currentRoute)) session>
               ...(
                    (~index, ~selectedVenture) =>
                      <Layout drawer=(currentRoute |> drawer(index))>
                        (currentRoute |> body(selectedVenture))
                      </Layout>
                  )
             </VentureStore>
         )
    </Router.Container>;
  },
};
