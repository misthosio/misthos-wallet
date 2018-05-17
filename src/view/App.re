include ViewCommon;

open Session;

let component = ReasonReact.statelessComponent("App");

let make = (~session, ~updateSession, _children) => {
  let onSignIn = _e => updateSession(SessionStore.SignIn);
  let onSignOut = _e => updateSession(SessionStore.SignOut);
  let onCloseModal = (ventureId, _) =>
    Router.Config.routeToUrl(Venture(ventureId, None))
    |> ReasonReact.Router.push;
  let modal =
      (
        selectedVenture: VentureStore.selectedVenture,
        currentRoute: Router.Config.route,
      ) =>
    switch (session, currentRoute, selectedVenture) {
    | (NotLoggedIn | LoginPending | AnonymousLogin | Unknown, _, _) => None
    | (
        LoggedIn(_),
        Venture(selected, ManagePartners),
        VentureLoaded(_, venture, commands),
      ) =>
      venture |> ViewModel.readOnly ?
        None :
        Some((
          <ManagePartnersModal
            viewData=(venture |> ViewModel.managePartnersModal)
            commands
          />,
          onCloseModal(selected),
        ))
    | (
        LoggedIn(_),
        Venture(selected, Receive),
        VentureLoaded(_, venture, commands),
      ) =>
      venture |> ViewModel.readOnly ?
        None : Some((<Receive commands />, onCloseModal(selected)))
    | (
        LoggedIn(_),
        Venture(selected, Payout),
        VentureLoaded(_, venture, commands),
      ) =>
      venture |> ViewModel.readOnly ?
        None :
        Some((
          <PayoutModal viewData=(venture |> ViewModel.payoutModal) commands />,
          onCloseModal(selected),
        ))
    | (LoggedIn(_), _, _) => None
    };
  let drawer = (index, currentRoute: Router.Config.route) =>
    switch (session, currentRoute) {
    | (NotLoggedIn | LoginPending | AnonymousLogin | Unknown, _) => None
    | (_, TypographyStack) => None
    | (LoggedIn(_data), Home | CreateVenture) =>
      Some(<Drawer onSignOut index />)
    | (LoggedIn(_data), Venture(selected, _) | JoinVenture(selected, _)) =>
      Some(<Drawer onSignOut selected index />)
    };
  let body =
      (
        selectedVenture: VentureStore.selectedVenture,
        createVenture,
        currentRoute: Router.Config.route,
      ) =>
    switch (session, currentRoute, selectedVenture) {
    | (NotLoggedIn, _, _) => <PublicHome onSignIn />
    | (_, TypographyStack, _) => <TypographyStack />
    | (Unknown, _, _) => <Spinner text="Loading" />
    | (AnonymousLogin, _, _) =>
      <Spinner
        text={js|
             You have signed in with a blockstack user that doesn't have a registered blockstack.id,
             make sure to upgrade the BlockStack client, close all Misthos tabs and try again with a registered id.
             |js}
      />
    | (LoginPending, _, _) =>
      <Spinner text="Waiting for BlockStack session" />
    | (
        LoggedIn(session),
        Venture(_, _) | Home | JoinVenture(_),
        VentureLoaded(_ventureId, venture, commands),
      ) =>
      <SelectedVenture
        viewData=(venture |> ViewModel.selectedVenture)
        commands
        session
      />
    | (LoggedIn(_), CreateVenture, _) =>
      <VentureCreate selectedVenture onCreateVenture=createVenture />
    | (LoggedIn(_), _, JoiningVenture(_)) =>
      <Spinner text="Joining venture" />
    | (LoggedIn(_), _, LoadingVenture(_)) =>
      <Spinner text="Loading venture" />
    | (LoggedIn(_), _, CreatingVenture) => <Spinner text="Creating venture" />
    | (LoggedIn(_), _, None) => text("Not selected")
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
                          modal=(currentRoute |> modal(selectedVenture))>
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
