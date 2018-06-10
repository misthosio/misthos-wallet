include ViewCommon;

let component = ReasonReact.statelessComponent("GlobalScroll");

module Styles = {
  open Css;
  let globalScroll =
    style([
      minWidth(px(Theme.space(101))),
      minHeight(px(Theme.space(88))),
    ]);
};

let make = children => {
  ...component,
  render: _self =>
    ReasonReact.createDomElement(
      "div",
      ~props={"className": Styles.globalScroll},
      children,
    ),
};
