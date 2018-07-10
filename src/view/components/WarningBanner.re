include ViewCommon;

let component = ReasonReact.statelessComponent("WarningBanner");

module Styles = {
  open Css;
  let warning = (~inline) =>
    style([
      fontFamily(Theme.sourceSansPro),
      fontSize(px(14)),
      fontWeight(700),
      color(Colors.white),
      textTransform(uppercase),
      padding2(~h=px(inline ? Theme.space(3) : 0), ~v=px(Theme.space(1))),
      marginBottom(px(inline ? Theme.space(3) : 0)),
      position(sticky),
      zIndex(10),
      top(px(0)),
      selector(
        "> a",
        [
          color(Colors.white),
          unsafe("textDecorationColor", Colors.uWhite),
          hover([color(Colors.misthosTeal)]),
        ],
      ),
    ]);
  let warningBg = style([unsafe("background", Colors.uGradientOrange)]);
};

let make = children => {
  ...component,
  render: _self =>
    <div className=(Styles.warning(~inline=true) ++ " " ++ Styles.warningBg)>
      children
    </div>,
};
