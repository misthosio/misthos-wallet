include ViewCommon;

[@bs.module "emotion"] external cssUnsafe: Js.t({..}) => string = "css";

module Styles = {
  open Css;

  let customScrollBar =
    cssUnsafe({
      "::-webkit-scrollbar-track": {
        "borderLeft": "9px solid white",
        "borderRight": "1px solid white",
        "backgroundColor": "#000",
      },
      "::-webkit-scrollbar": {
        "width": "11px",
        "backgroundColor": "#fff",
      },
      "::-webkit-scrollbar-thumb": {
        "borderLeft": "8px solid white",
        "backgroundColor": "#000",
      },
    });

  let scrollContainer =
    style([
      unsafe("flex", "0 1 auto"),
      overflowX(hidden),
      overflowY(auto),
      minHeight(px(0)),
      width(`percent(100.0)),
    ]);
};

let containerStyles =
  Css.(
    style([
      height(`percent(100.0)),
      display(`flex),
      flexDirection(column),
    ])
  );

let component = ReasonReact.statelessComponent("ScrollList");

let make = children => {
  ...component,
  render: _self =>
    <div className={Styles.scrollContainer ++ " " ++ Styles.customScrollBar}>
      ...children
    </div>,
};
