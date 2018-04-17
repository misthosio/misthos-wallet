open Session;

let component = ReasonReact.statelessComponent("App");

let make = (~session, ~updateSession, _children) => {
  ...component,
  render: _self => {
    let onSignIn = _e => updateSession(SessionStore.SignIn);
    let onSignOut = _e => updateSession(SessionStore.SignOut);
    let drawer = (index, currentRoute: Router.Config.route) =>
      switch (session, currentRoute) {
      | (NotLoggedIn | LoginPending | AnonymousLogin | Unknown, _) => None
      | (LoggedIn(_data), Home) => Some(<Drawer onSignOut index />)
      | (LoggedIn(_data), Venture(selected)) =>
        Some(<Drawer onSignOut selected index />)
      | (LoggedIn(_data), JoinVenture(selected, _)) =>
        Some(<Drawer onSignOut selected index />)
      };
    let body =
        (
          selectedVenture,
          updateVentureStore,
          currentRoute: Router.Config.route,
        ) =>
      switch (session, currentRoute) {
      | (NotLoggedIn | LoginPending | AnonymousLogin | Unknown, _) =>
        <PublicHome onSignIn />
      | (LoggedIn(session), Home) =>
        <Home session selectedVenture updateVentureStore />
      | (LoggedIn(session), _) =>
        <Home session selectedVenture updateVentureStore />
      };
    <Router.Container>
      ...(
           (~currentRoute) =>
             <VentureStore currentRoute session>
               ...(
                    (~index, ~selectedVenture, ~updateVentureStore) =>
                      <Layout drawer=(currentRoute |> drawer(index))>
                        (
                          currentRoute
                          |> body(selectedVenture, updateVentureStore)
                        )
                      </Layout>
                  )
             </VentureStore>
         )
    </Router.Container>;
  },
};
