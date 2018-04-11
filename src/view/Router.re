let str = ReasonReact.stringToElement;

open Session;

module RouterConfig = {
  type route =
    | Home;
  let routeFromUrl = (url: ReasonReact.Router.url) =>
    switch (url.path) {
    | [] => Home
    | _ => Home
    };
  let routeToUrl = (route: route) =>
    switch (route) {
    | Home => "/"
    };
};

module Router = ReRoute.CreateRouter(RouterConfig);

let component = ReasonReact.statelessComponent("Router");

let make = (~session, ~updateSession, _children) => {
  ...component,
  render: _self => {
    let drawer = currentRoute =>
      switch (session, currentRoute) {
      | (NotLoggedIn | LoginPending | AnonymousLogin, _) => None
      | (Session.LoggedIn(data), RouterConfig.Home) =>
        Some(<VentureList session />)
      };
    let body = currentRoute =>
      switch (session, currentRoute) {
      | (NotLoggedIn | LoginPending | AnonymousLogin, _) =>
        <PublicHome onSignIn=(_e => updateSession(UserSession.SignIn)) />
      | (Session.LoggedIn(data), RouterConfig.Home) => <Home data />
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
