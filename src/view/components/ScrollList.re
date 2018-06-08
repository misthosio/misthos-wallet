include ViewCommon;

[@bs.module "glamor"] external cssUnsafe : Js.t({..}) => string = "css";

module Styles = {
  open Css;

  let customScrollBar =
    cssUnsafe({
      "::-webkit-scrollbar-track": {
        "width": "10px",
        "borderLeft": "1px solid white",
        "borderRight": "1px solid white",
        "backgroundColor": "#000",
      },
      "::-webkit-scrollbar": {
        "width": "3px",
        "backgroundColor": "#fff",
      },
      "::-webkit-scrollbar-thumb": {
        "backgroundColor": "#000",
      },
    });

  let scrollContainer =
    style([
      position(relative),
      overflow(hidden),
      width(`percent(100.0)),
      height(auto),
      minHeight(`percent(100.0)),
      maxHeight(`percent(100.0)),
    ]);
};

let containerStyles =
  Css.(
    style([
      height(`percent(100.0)),
      display(`flex),
      flexDirection(column),
    ])
  );

let component = ReasonReact.statelessComponent("ScrollList");

let make = children => {
  ...component,
  render: _self =>
    ReasonReact.createDomElement(
      "div",
      ~props={
        "className": Styles.scrollContainer ++ " " ++ Styles.customScrollBar,
      },
      children,
    ),
};
