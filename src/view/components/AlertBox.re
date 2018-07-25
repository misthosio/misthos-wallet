include ViewCommon;

type icon =
  | Plus
  | Minus
  | ArrowUp;

let component = ReasonReact.statelessComponent("AlertBox");

module Styles = {
  open Css;
  let alert =
    style([
      margin2(~v=px(Theme.space(2)), ~h=px(0)),
      padding(px(Theme.space(2))),
      borderStyle(solid),
      borderWidth(px(2)),
      unsafe("borderImageSlice", "1"),
      unsafe("borderImageSource", Colors.uGradientOrange),
      marginBottom(px(Theme.space(1))),
      display(`flex),
      flexDirection(column),
    ]);
};

let make = children => {
  ...component,
  render: _self =>
    ReasonReact.createDomElement(
      "div",
      ~props={"className": Styles.alert},
      children,
    ),
};
