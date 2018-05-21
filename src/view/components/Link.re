let component = ReasonReact.statelessComponent("Link");

let make = (~route, ~className=?, children) => {
  ...component,
  render: _self => {
    let href = Router.Config.routeToUrl(route);
    <MaterialUi.Button href ?className onClick=(Router.clickToRoute(route))>
      (ReasonReact.array(children))
    </MaterialUi.Button>;
  },
};
