let component = ReasonReact.statelessComponent("MFabButton");

[@bs.module "glamor"] external cssUnsafe : Js.t({..}) => string = "css";

type variant =
  | Aqua
  | Orange;

module Styles = {
  open Css;
  let button = (variant: variant) =>
    style([
      margin2(~v=px(Theme.space(5)), ~h=px(0)),
      width(px(Theme.space(19))),
      height(px(Theme.space(19))),
      borderRadius(px(Theme.space(19))),
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
    ]);
};

let make = (~variant, ~onClick=?, children) => {
  ...component,
  render: _self =>
    <MaterialUi.Button
      className=(Styles.button(variant)) variant=`Fab ?onClick>
      children
    </MaterialUi.Button>,
};
