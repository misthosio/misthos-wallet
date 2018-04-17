open PrimitiveTypes;

module Config = {
  type route =
    | Home
    | Venture(ventureId)
    | JoinVenture(ventureId, userId);
  let routeFromUrl = (url: ReasonReact.Router.url) =>
    switch (url.path) {
    | ["ventures", id] => Venture(id |> VentureId.fromString)
    | ["ventures", id, "joinvia", userId] =>
      JoinVenture(id |> VentureId.fromString, userId |> UserId.fromString)
    | [] => Home
    | _ => Home
    };
  let routeToUrl = (route: route) =>
    switch (route) {
    | Venture(id) => "/ventures/" ++ (id |> VentureId.toString)
    | JoinVenture(id, userId) =>
      "/ventures/"
      ++ (id |> VentureId.toString)
      ++ "/joinvia/"
      ++ (userId |> UserId.toString)
    | Home => "/"
    };
};

include ReRoute.CreateRouter(Config);
