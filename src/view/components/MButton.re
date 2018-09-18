let component = ReasonReact.statelessComponent("MButton");

type variant =
  | Flat
  | Outlined;

module Styles = {
  open Css;
  let button = (fullWidth, variant, gutterTop, gutterBottom) => {
    let baseRules = [
      width(fullWidth ? `percent(100.0) : auto),
      marginTop(px(Theme.space(gutterTop ? 5 : 0))),
      marginBottom(px(Theme.space(gutterBottom ? 5 : 0))),
    ];
    let variantRules =
      switch (variant) {
      | Flat => [
          textDecoration(underline),
          hover([textDecoration(underline)]),
          paddingLeft(px(Theme.space(1))),
          paddingRight(px(Theme.space(1))),
          unsafe("minWidth", "min-content"),
        ]
      | Outlined => [
          borderRadius(px(25)),
          border(px(2), `solid, black),
          paddingLeft(px(25)),
          paddingRight(px(25)),
          minHeight(px(45)),
          maxHeight(px(45)),
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
      ~gutterTop=true,
      ~gutterBottom=false,
      ~href=?,
      ~targetBlank=false,
      ~submitBtn=false,
      children,
    ) => {
  ...component,
  render: _self => {
    let callback = (~props) =>
      ReasonReact.cloneElement(<a target="_blank" />, ~props, [||]);
    let component = targetBlank ? Some(`Callback(callback)) : None;
    let button =
      <MaterialUi.Button
        ?size
        className=(
          Styles.button(fullWidth, variant, gutterTop, gutterBottom)
          ++ " "
          ++ className
        )
        ?color
        ?href
        ?component
        ?onClick>
        ...children
      </MaterialUi.Button>;
    submitBtn ?
      button |. ReasonReact.cloneElement(~props={"type": "submit"}, [||]) :
      button;
  },
};
