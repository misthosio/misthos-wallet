include ViewCommon;

module Styles = {
  open Css;
  let view =
    style([
      position(relative),
      overflow(hidden),
      width(`percent(100.0)),
      height(auto),
      minHeight(`percent(100.0)),
      maxHeight(`percent(100.0)),
    ]);
  let flexContainer = style([flex(1), minHeight(px(0))]);
};

let containerStyles =
  Css.(
    style([
      height(`percent(100.0)),
      display(`flex),
      flexDirection(column),
    ])
  );

module CustomScrollbar = {
  [@bs.module "react-custom-scrollbars"]
  external reactClass : ReasonReact.reactClass = "default";

  let make = children =>
    ReasonReact.wrapJsForReason(~reactClass, ~props={}, children);
};

let component = ReasonReact.statelessComponent("ScrollList");

let make = children => {
  ...component,
  render: _self =>
    <div className=Styles.flexContainer>
      <CustomScrollbar> children </CustomScrollbar>
    </div>,
};
