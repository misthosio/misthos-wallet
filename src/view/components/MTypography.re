let statelessComponent = ReasonReact.statelessComponent("MTypography");

module Styles = {
  open Css;
  let margin = top => style([marginTop(px(Theme.space(top)))]);
};

let make =
    (
      ~variant,
      ~className="",
      ~gutterBottom=false,
      ~gutterTop=false,
      ~component=?,
      children,
    ) => {
  ...statelessComponent,
  render: _self => {
    let style =
      switch (variant, gutterTop) {
      | (`Title, true)
      | (`Headline, true) => Styles.margin(4)
      | (_, _) => Styles.margin(0)
      };
    <MaterialUi.Typography
      ?component className=(style ++ " " ++ className) variant gutterBottom>
      children
    </MaterialUi.Typography>;
  },
};
