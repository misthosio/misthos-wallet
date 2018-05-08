let component = ReasonReact.statelessComponent("MInput");

module Styles = {
  open Css;
  let margin = (~tf, ~bf) =>
    style([
      marginTop(px(Theme.space(tf))),
      marginBottom(px(Theme.space(bf))),
    ]);
};

let make =
    (
      ~placeholder=?,
      ~value=?,
      ~onChange=?,
      ~autoFocus=?,
      ~fullWidth=?,
      ~ensuring=false,
      _children,
    ) => {
  ...component,
  render: _self =>
    <MaterialUi.Input
      className=(Styles.margin(~tf=ensuring ? 4 : 3, ~bf=0))
      ?placeholder
      ?value
      ?onChange
      ?autoFocus
      ?fullWidth
    />,
};
