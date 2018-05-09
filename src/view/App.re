open Session;

let component = ReasonReact.statelessComponent("App");

let make = (~session, ~updateSession, _children) => {
  let onSignIn = _e => updateSession(SessionStore.SignIn);
  let onSignOut = _e => updateSession(SessionStore.SignOut);
  let onCloseModal = (ventureId, unit) =>
    Router.Config.routeToUrl(Venture(ventureId)) |> ReasonReact.Router.push;
  let modal = (currentRoute: Router.Config.route) =>
    switch (session, currentRoute) {
    | (NotLoggedIn | LoginPending | AnonymousLogin | Unknown, _) => None
    | (LoggedIn(session), ManagePartners(selected)) =>
      Some((<Spinner text="manage partners" />, onCloseModal(selected)))
    | (LoggedIn(session), _) => None
    };
  let drawer = (index, currentRoute: Router.Config.route) =>
    switch (session, currentRoute) {
    | (NotLoggedIn | LoginPending | AnonymousLogin | Unknown, _) => None
    | (_, TypographyStack) => None
    | (LoggedIn(_data), Home | CreateVenture) =>
      Some(<Drawer onSignOut index />)
    | (
        LoggedIn(_data),
        Venture(selected) | JoinVenture(selected, _) |
        ManagePartners(selected),
      ) =>
      Some(<Drawer onSignOut selected index />)
    };
  let body =
      (selectedVenture, createVenture, currentRoute: Router.Config.route) =>
    switch (session, currentRoute) {
    | (NotLoggedIn, _) => <PublicHome onSignIn />
    | (_, TypographyStack) => <TypographyStack />
    | (Unknown, _) => <Spinner text="Loading" />
    | (AnonymousLogin, _) =>
      <Spinner
        text={js|
             You have signed in with a blockstack user that doesn't have a registered blockstack.id,
             make sure to upgrade the BlockStack client, close all Misthos tabs and try again with a registered id.
             |js}
      />
    | (LoginPending, _) => <Spinner text="Waiting for BlockStack session" />
    | (LoggedIn(session), Home | ManagePartners(_)) =>
      <Home session selectedVenture />
    | (LoggedIn(_), CreateVenture) =>
      <VentureCreate selectedVenture onCreateVenture=createVenture />
    | (LoggedIn(session), _) => <Home session selectedVenture />
    };
  {
    ...component,
    render: _self =>
      <Router.Container>
        ...(
             (~currentRoute) =>
               <VentureStore currentRoute session>
                 ...(
                      (~index, ~selectedVenture, ~createVenture) =>
                        <Layout
                          drawer=(currentRoute |> drawer(index))
                          modal=(currentRoute |> modal)>
                          ...(
                               currentRoute
                               |> body(selectedVenture, createVenture)
                             )
                        </Layout>
                    )
               </VentureStore>
           )
      </Router.Container>,
  };
};
