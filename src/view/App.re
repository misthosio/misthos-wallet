include ViewCommon;

open Session;

let component = ReasonReact.statelessComponent("App");

let make = (~session, ~updateSession, _children) => {
  let onSignIn = _e => updateSession(SessionStore.SignIn);
  let onSignOut = _e => updateSession(SessionStore.SignOut);
  let onCloseModal = (ventureId, _e) =>
    Router.Config.routeToUrl(Venture(ventureId, None))
    |> ReasonReact.Router.push;
  let mobileEnabled =
    switch (session) {
    | NotLoggedIn => true
    | _ => false
    };
  let modal =
      (
        selectedVenture: VentureStore.selectedVenture,
        currentRoute: Router.Config.route,
      ) =>
    switch (session, currentRoute, selectedVenture) {
    | (NotLoggedIn | LoginPending | NamelessLogin | Unknown, _, _) => None
    | (
        LoggedIn(_),
        Venture(selected, ManagePartners),
        VentureLoaded(_, venture, commands),
      ) =>
      venture |> ViewModel.readOnly ?
        None :
        Some((
          <CommandExecutor
            commands lastResponse=(venture |> ViewModel.lastResponse)>
            ...(
                 (
                   ~commands as proposePartnerCmds,
                   ~cmdStatus as proposeCmdStatus,
                 ) =>
                   <CommandExecutor
                     commands lastResponse=(venture |> ViewModel.lastResponse)>
                     ...(
                          (
                            ~commands as removePartnerCmds,
                            ~cmdStatus as removeCmdStatus,
                          ) =>
                            <ManagePartnersModal
                              viewData=(
                                venture |> ViewModel.managePartnersModal
                              )
                              proposePartnerCmds
                              proposeCmdStatus
                              removePartnerCmds
                              removeCmdStatus
                            />
                        )
                   </CommandExecutor>
               )
          </CommandExecutor>,
          onCloseModal(selected),
        ))
    | (
        LoggedIn(_),
        Venture(selected, Partner(processId)),
        VentureLoaded(_, venture, commands),
      ) =>
      venture |> ViewModel.readOnly ?
        None :
        Some((
          switch (venture |> ViewModel.viewPartnerModal(processId)) {
          | Some(viewData) =>
            <CommandExecutor
              commands lastResponse=(venture |> ViewModel.lastResponse)>
              ...(
                   (~commands, ~cmdStatus) =>
                     <ViewPartnerModal commands cmdStatus viewData />
                 )
            </CommandExecutor>
          | None => <NotFoundModal resource=NotFoundModal.Partner />
          },
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
        Venture(selected, CreatePayout),
        VentureLoaded(ventureId, venture, commands),
      ) =>
      venture |> ViewModel.readOnly ?
        None :
        Some((
          <CommandExecutor
            commands
            lastResponse=(venture |> ViewModel.lastResponse)
            onProcessStarted=(
              processId =>
                Router.goTo(Venture(ventureId, Payout(processId)))
            )>
            ...(
                 (~commands, ~cmdStatus) =>
                   <CreatePayoutModal
                     viewData=(venture |> ViewModel.createPayoutModal)
                     commands
                     cmdStatus
                   />
               )
          </CommandExecutor>,
          onCloseModal(selected),
        ))
    | (
        LoggedIn(_),
        Venture(selected, Payout(processId)),
        VentureLoaded(_, venture, commands),
      ) =>
      venture |> ViewModel.readOnly ?
        None :
        Some((
          switch (venture |> ViewModel.viewPayoutModal(processId)) {
          | Some(viewData) =>
            <CommandExecutor
              commands lastResponse=(venture |> ViewModel.lastResponse)>
              ...(
                   (~commands, ~cmdStatus) =>
                     <ViewPayoutModal viewData commands cmdStatus />
                 )
            </CommandExecutor>
          | None => <NotFoundModal resource=NotFoundModal.Payout />
          },
          onCloseModal(selected),
        ))
    | (
        LoggedIn(_),
        Venture(selected, Income(txId)),
        VentureLoaded(_, venture, _),
      ) =>
      venture |> ViewModel.readOnly ?
        None :
        Some((
          switch (venture |> ViewModel.viewIncomeModal(txId)) {
          | Some(viewData) => <ViewIncomeModal viewData />
          | None => <NotFoundModal resource=NotFoundModal.Income />
          },
          onCloseModal(selected),
        ))
    | (LoggedIn(_), _, _) => None
    };
  let drawer = (index, currentRoute: Router.Config.route) =>
    switch (session, currentRoute) {
    | (NotLoggedIn | LoginPending | NamelessLogin | Unknown, _) => None
    | (_, TypographyStack) => None
    | (LoggedIn(_data), Home | CreateVenture) =>
      Some(<Drawer onSignOut index />)
    | (LoggedIn(_data), Venture(selected, _) | JoinVenture(selected, _)) =>
      Some(<Drawer onSignOut selected index />)
    };
  let body =
      (
        index,
        selectedVenture: VentureStore.selectedVenture,
        createVenture,
        currentRoute: Router.Config.route,
      ) =>
    switch (session, currentRoute, selectedVenture) {
    | (NotLoggedIn, _, _) => <div> <PublicHome onSignIn /> <Footer /> </div>
    | (_, TypographyStack, _) => <TypographyStack />
    | (Unknown, _, _) => <BlankScreen text="Loading" />
    | (NamelessLogin, _, _) => <NamelessLogin />
    | (LoginPending, _, _) =>
      <BlankScreen text="Waiting for Blockstack session" />
    | (
        LoggedIn(_),
        Venture(_, _) | Home | JoinVenture(_),
        VentureLoaded(_ventureId, venture, _),
      ) =>
      <SelectedVenture viewData=(venture |> ViewModel.selectedVenture) />
    | (LoggedIn(_), CreateVenture, _)
    | (LoggedIn(_), _, CreatingVenture(_)) =>
      let cmdStatus =
        switch (selectedVenture) {
        | CreatingVenture(cmdStatus) => cmdStatus
        | _ => Idle
        };
      <VentureCreate cmdStatus onCreateVenture=createVenture />;
    | (LoggedIn(_), _, JoiningVenture(_, cmdStatus)) =>
      <JoinVenture cmdStatus />
    | (LoggedIn(_), _, LoadingVenture(_, cmdStatus)) =>
      <LoadVenture cmdStatus />
    | (LoggedIn(_), _, None) => <LoggedInHome index />
    };
  {
    ...component,
    render: _self =>
      <Router.Container>
        ...(
             (~currentRoute) =>
               <VentureStore currentRoute session>
                 ...(
                      (~index, ~selectedVenture, ~createVenture) => {
                        let drawer = currentRoute |> drawer(index);
                        let modal = currentRoute |> modal(selectedVenture);
                        <Layout mobileEnabled ?drawer ?modal>
                          (
                            currentRoute
                            |> body(index, selectedVenture, createVenture)
                          )
                        </Layout>;
                      }
                    )
               </VentureStore>
           )
      </Router.Container>,
  };
};
