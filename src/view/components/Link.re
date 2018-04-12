let component = ReasonReact.statelessComponent("Link");

let make = (~route, ~className=?, children) => {
  ...component,
  render: self => {
    let href = Router.Config.routeToUrl(route);
    <a
      href
      ?className
      onClick=(
        event => {
          ReactEventRe.Synthetic.preventDefault(event);
          ReasonReact.Router.push(href);
        }
      )>
      (ReasonReact.arrayToElement(children))
    </a>;
  },
};
