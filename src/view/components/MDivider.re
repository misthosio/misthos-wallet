let component = ReasonReact.statelessComponent("MDivider");

type variant =
  | Aqua
  | Orange;

module Styles = {
  open Css;
  let divider =
    style([
      borderRadius(px(4)),
      backgroundColor(Colors.black),
      boxShadow(~blur=px(5), rgba(0, 0, 0, 0.02)),
    ]);
};

let make = _children => {
  ...component,
  render: _self =>
    <MaterialUi.Divider component=(`String("li")) className=Styles.divider />,
};
