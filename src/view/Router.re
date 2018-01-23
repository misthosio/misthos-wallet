module Link = {
  let component = ReasonReact.statelessComponent("Router.Link");
  let make = (~className="", ~href, children) => {
    let handleClick = e => {
      ReactEventRe.Mouse.preventDefault(e);
      ReasonReact.Router.push(href);
    };
    {
      ...component,
      render: (_) =>
        ReasonReact.createDomElement(
          "a",
          ~props={
            "className": className,
            "href": href,
            "onClick": handleClick
          },
          children
        )
    };
  };
};

type state = {url: ReasonReact.Router.url};

type action =
  | UpdateUrl(ReasonReact.Router.url);

let component = ReasonReact.reducerComponent("Router");

let make = renderChildren => {
  ...component,
  initialState: () => {
    url: {
      path: [],
      hash: "",
      search: ""
    }
  },
  subscriptions: ({send}) => [
    Sub(
      () => ReasonReact.Router.watchUrl(url => send(UpdateUrl(url))),
      ReasonReact.Router.unwatchUrl
    )
  ],
  reducer: (action, _state) =>
    switch action {
    | UpdateUrl(url) => ReasonReact.Update({url: url})
    },
  render: ({state}) => renderChildren(state.url)
};
