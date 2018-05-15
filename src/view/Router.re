open PrimitiveTypes;

module Config = {
  type ventureSubRoute =
    | None
    | ManagePartners
    | Payout
    | Receive;
  type route =
    | Home
    | Venture(ventureId, ventureSubRoute)
    | JoinVenture(ventureId, userId)
    | CreateVenture
    | TypographyStack;
  let routeFromUrl = (url: ReasonReact.Router.url) =>
    switch (url.path) {
    | ["ventures", "new"] => CreateVenture
    | ["ventures", id] => Venture(id |> VentureId.fromString, None)
    | ["ventures", id, "partners"] =>
      Venture(id |> VentureId.fromString, ManagePartners)
    | ["ventures", id, "payout"] =>
      Venture(id |> VentureId.fromString, Payout)
    | ["ventures", id, "receive"] =>
      Venture(id |> VentureId.fromString, Receive)
    | ["ventures", id, "joinvia", userId] =>
      JoinVenture(id |> VentureId.fromString, userId |> UserId.fromString)
    | ["typographystack"] => TypographyStack
    | [] => Home
    | _ => Home
    };
  let routeToUrl = (route: route) =>
    switch (route) {
    | CreateVenture => "/ventures/new"
    | Venture(id, None) => "/ventures/" ++ (id |> VentureId.toString)
    | Venture(id, ManagePartners) =>
      "/ventures/" ++ (id |> VentureId.toString) ++ "/partners"
    | Venture(id, Payout) =>
      "/ventures/" ++ (id |> VentureId.toString) ++ "/payout"
    | Venture(id, Receive) =>
      "/ventures/" ++ (id |> VentureId.toString) ++ "/receive"
    | JoinVenture(id, userId) =>
      "/ventures/"
      ++ (id |> VentureId.toString)
      ++ "/joinvia/"
      ++ (userId |> UserId.toString)
    | TypographyStack => "/typographystack"
    | Home => "/"
    };
};

include ReRoute.CreateRouter(Config);

let goTo = route => ReasonReact.Router.push(Config.routeToUrl(route));
