let component = ReasonReact.statelessComponent("LinkButton");

let make = (~route, ~fullWidth=false, children) => {
  ...component,
  render: _self => {
    let href = Router.Config.routeToUrl(route);
    <MButton
      fullWidth
      onClick=(
        event => {
          ReactEventRe.Synthetic.preventDefault(event);
          ReasonReact.Router.push(href);
        }
      )>
      (ReasonReact.array(children))
    </MButton>;
  },
};
