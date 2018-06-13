let component = ReasonReact.statelessComponent("MTypography");

module Styles = {
  open Css;
  let margin = (~tf, ~bf) =>
    style([
      marginTop(px(Theme.space(tf))),
      marginBottom(px(Theme.space(bf))),
    ]);
};

let make = (~variant, ~className="", children) => {
  ...component,
  render: _self => {
    let style =
      switch (variant) {
      | `Headline => Styles.margin(~tf=4, ~bf=4)
      | `Title => Styles.margin(~tf=4, ~bf=0)
      | _ => Styles.margin(~tf=0, ~bf=0)
      };
    <MaterialUi.Typography className=(style ++ " " ++ className) variant>
      children
    </MaterialUi.Typography>;
  },
};
