let component = ReasonReact.statelessComponent("MButton");

type variant =
  | Flat
  | Outlined;

module Styles = {
  open Css;
  let button = (fullWidth, variant) => {
    let baseRules = [
      width(fullWidth ? `percent(100.0) : auto),
      margin2(~v=px(Theme.space(5)), ~h=px(0)),
    ];
    let variantRules =
      switch (variant) {
      | Flat => [
          textDecoration(underline),
          hover([textDecoration(underline)]),
          paddingLeft(px(10)),
          paddingRight(px(10)),
        ]
      | Outlined => [
          borderRadius(px(25)),
          border(px(2), `solid, black),
          paddingLeft(px(25)),
          paddingRight(px(25)),
        ]
      };
    style([baseRules, variantRules] |> List.flatten);
  };
};

let make =
    (
      ~color=?,
      ~onClick=?,
      ~size=?,
      ~fullWidth=false,
      ~variant=Outlined,
      ~className="",
      children,
    ) => {
  ...component,
  render: _self =>
    <MaterialUi.Button
      ?size
      className=(Styles.button(fullWidth, variant) ++ " " ++ className)
      ?color
      ?onClick>
      children
    </MaterialUi.Button>,
};
