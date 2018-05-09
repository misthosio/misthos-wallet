open PrimitiveTypes;

module Config = {
  type route =
    | Home
    | Venture(ventureId)
    | JoinVenture(ventureId, userId)
    | ManagePartners(ventureId)
    | CreateVenture
    | TypographyStack;
  let routeFromUrl = (url: ReasonReact.Router.url) =>
    switch (url.path) {
    | ["ventures", "new"] => CreateVenture
    | ["ventures", id] => Venture(id |> VentureId.fromString)
    | ["ventures", id, "partners"] =>
      ManagePartners(id |> VentureId.fromString)
    | ["ventures", id, "joinvia", userId] =>
      JoinVenture(id |> VentureId.fromString, userId |> UserId.fromString)
    | ["typographystack"] => TypographyStack
    | [] => Home
    | _ => Home
    };
  let routeToUrl = (route: route) =>
    switch (route) {
    | CreateVenture => "/ventures/new"
    | Venture(id) => "/ventures/" ++ (id |> VentureId.toString)
    | ManagePartners(id) =>
      "/ventures/" ++ (id |> VentureId.toString) ++ "/partners"
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
