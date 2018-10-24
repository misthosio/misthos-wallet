open PrimitiveTypes;

module Config = {
  type ventureSubRoute =
    | None
    | Settings
    | ManagePartners
    | CreatePayout
    | Partner(processId)
    | Payout(processId)
    | Income(string)
    | HiddenOutputLog
    | Addresses
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
    | ["ventures", id, "addresses"] =>
      Venture(id |> VentureId.fromString, Addresses)

    | ["ventures", id, "partners", processId] =>
      Venture(
        id |> VentureId.fromString,
        Partner(processId |> ProcessId.fromString),
      )
    | ["ventures", id, "partners"] =>
      Venture(id |> VentureId.fromString, ManagePartners)
    | ["ventures", id, "hidden", "log"] =>
      Venture(id |> VentureId.fromString, HiddenOutputLog)
    | ["ventures", id, "payouts", "new"] =>
      Venture(id |> VentureId.fromString, CreatePayout)
    | ["ventures", id, "payouts", processId] =>
      Venture(
        id |> VentureId.fromString,
        Payout(processId |> ProcessId.fromString),
      )
    | ["ventures", id, "income", transactionId] =>
      Venture(id |> VentureId.fromString, Income(transactionId))
    | ["ventures", id, "receive"] =>
      Venture(id |> VentureId.fromString, Receive)
    | ["ventures", id, "settings"] =>
      Venture(id |> VentureId.fromString, Settings)
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
    | Venture(id, Addresses) =>
      "/ventures/" ++ (id |> VentureId.toString) ++ "/addresses"
    | Venture(id, ManagePartners) =>
      "/ventures/" ++ (id |> VentureId.toString) ++ "/partners"
    | Venture(id, HiddenOutputLog) =>
      "/ventures/" ++ (id |> VentureId.toString) ++ "/hidden/log"
    | Venture(id, Partner(processId)) =>
      "/ventures/"
      ++ (id |> VentureId.toString)
      ++ "/partners/"
      ++ (processId |> ProcessId.toString)
    | Venture(id, CreatePayout) =>
      "/ventures/" ++ (id |> VentureId.toString) ++ "/payouts/new"
    | Venture(id, Payout(processId)) =>
      "/ventures/"
      ++ (id |> VentureId.toString)
      ++ "/payouts/"
      ++ (processId |> ProcessId.toString)
    | Venture(id, Income(txId)) =>
      "/ventures/" ++ (id |> VentureId.toString) ++ "/income/" ++ txId
    | Venture(id, Receive) =>
      "/ventures/" ++ (id |> VentureId.toString) ++ "/receive"
    | Venture(id, Settings) =>
      "/ventures/" ++ (id |> VentureId.toString) ++ "/settings"
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
  ReactEvent.Synthetic.preventDefault(event);
  goTo(route);
};
