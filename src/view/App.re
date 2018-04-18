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
      | (NotLoggedIn, _) => <PublicHome onSignIn />
      | (Unknown, _) => <Spinner text="Loading" />
      | (AnonymousLogin, _) =>
        <Spinner
          text="Missing BlockStack session, upgrade BlockStack client, close all Misthos tabs and try again"
        />
      | (LoginPending, _) => <Spinner text="Waiting for BlockStack session" />
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
