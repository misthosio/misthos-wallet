open PrimitiveTypes;

module Config = {
  type ventureSubRoute =
    | None
    | ManagePartners
    | CreatePayout
    | Partner(userId)
    | Payout(processId)
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
    | ["ventures", id, "partners", partnerId] =>
      Venture(
        id |> VentureId.fromString,
        Partner(partnerId |> UserId.fromString),
      )
    | ["ventures", id, "partners"] =>
      Venture(id |> VentureId.fromString, ManagePartners)
    | ["ventures", id, "payouts", "new"] =>
      Venture(id |> VentureId.fromString, CreatePayout)
    | ["ventures", id, "payouts", processId] =>
      Venture(
        id |> VentureId.fromString,
        Payout(processId |> ProcessId.fromString),
      )
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
    | Venture(id, Partner(partnerId)) =>
      "/ventures/"
      ++ (id |> VentureId.toString)
      ++ "/partners/"
      ++ (partnerId |> UserId.toString)
    | Venture(id, CreatePayout) =>
      "/ventures/" ++ (id |> VentureId.toString) ++ "/payouts/new"
    | Venture(id, Payout(processId)) =>
      "/ventures/"
      ++ (id |> VentureId.toString)
      ++ "/payouts/"
      ++ (processId |> ProcessId.toString)
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

let clickToRoute = (route, event) => {
  ReactEventRe.Synthetic.preventDefault(event);
  goTo(route);
};
