open PrimitiveTypes;

module Config = {
  type route =
    | Home
    | Venture(ventureId);
  let routeFromUrl = (url: ReasonReact.Router.url) =>
    switch (url.path) {
    | ["ventures", id] => Venture(id |> VentureId.fromString)
    | [] => Home
    | _ => Home
    };
  let routeToUrl = (route: route) =>
    switch (route) {
    | Venture(id) => "/ventures/" ++ (id |> VentureId.toString)
    | Home => "/"
    };
};

include ReRoute.CreateRouter(Config);
