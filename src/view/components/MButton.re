let component = ReasonReact.statelessComponent("MButton");

module Styles = {
  open Css;
  let button = fullWidth =>
    style([
      borderRadius(px(25)),
      border(px(2), `solid, black),
      margin(px(2)),
      paddingLeft(px(25)),
      paddingRight(px(25)),
      width(fullWidth ? `percent(100.0) : auto),
    ]);
};

let make = (~color=?, ~onClick=?, ~fullWidth=false, children) => {
  ...component,
  render: _self =>
    <MaterialUi.Button className=(Styles.button(fullWidth)) ?color ?onClick>
      children
    </MaterialUi.Button>,
};
