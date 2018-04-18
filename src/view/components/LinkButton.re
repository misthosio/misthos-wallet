let component = ReasonReact.statelessComponent("LinkButton");

let make = (~route, children) => {
  ...component,
  render: _self => {
    let href = Router.Config.routeToUrl(route);
    <MButton
      onClick=(
        event => {
          ReactEventRe.Synthetic.preventDefault(event);
          ReasonReact.Router.push(href);
        }
      )>
      (ReasonReact.arrayToElement(children))
    </MButton>;
  },
};
