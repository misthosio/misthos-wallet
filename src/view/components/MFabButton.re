include ViewCommon;

let component = ReasonReact.statelessComponent("MFabButton");

type variant =
  | Aqua
  | Orange;

module Styles = {
  open Css;
  open BreakPoints;
  let button = (variant: variant) =>
    style([
      sm([width(px(Theme.space(19))), height(px(Theme.space(19)))]),
      xs([width(px(Theme.space(14))), height(px(Theme.space(14)))]),
      borderRadius(px(Theme.space(19))),
      fontSize(px(16)),
      unsafe("boxShadow", "none"),
      unsafe("border", "double 4px transparent"),
      unsafe("borderImageSlice", "1"),
      unsafe(
        "backgroundImage",
        "linear-gradient(white, white), "
        ++ (
          switch (variant) {
          | Aqua => Colors.uGradientAqua
          | Orange => Colors.uGradientOrange
          }
        ),
      ),
      unsafe("backgroundOrigin", "border-box"),
      unsafe("backgroundClip", "content-box, border-box"),
      active([
        unsafe(
          "backgroundImage",
          "linear-gradient(rgba(255, 255, 255, 0.3), rgba(255, 255, 255, 0.3)), "
          ++ (
            switch (variant) {
            | Aqua => Colors.uGradientAqua
            | Orange => Colors.uGradientOrange
            }
          ),
        ),
        unsafe("border", "none"),
        unsafe("boxShadow", "none"),
      ]),
      hover([
        unsafe(
          "backgroundImage",
          switch (variant) {
          | Aqua => Colors.uGradientAqua
          | Orange => Colors.uGradientOrange
          },
        ),
        unsafe("border", "none"),
      ]),
    ]);
};

let make = (~variant, ~route, children) => {
  ...component,
  render: _self => {
    let href = Router.Config.routeToUrl(route);
    <MaterialUi.Button
      className=(Styles.button(variant))
      variant=`Fab
      focusRipple=false
      onClick=(ignoreEvent(() => ReasonReact.Router.push(href)))>
      children
    </MaterialUi.Button>;
  },
};
