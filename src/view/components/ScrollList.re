include ViewCommon;

[@bs.module "react-custom-scrollbars"]
external reactClass : ReasonReact.reactClass = "default";

module Styles = {
  open Css;
  let foo = style([]);
};

[@bs.deriving abstract]
type jsProps = {
  renderView: unit => ReasonReact.reactElement,
  renderThumb: unit => ReasonReact.reactElement,
};

let renderView = () => <div className=Styles.foo />;

let make = children =>
  ReasonReact.wrapJsForReason(
    ~reactClass,
    ~props={"autoHeight": true, "autoHide": true},
    children,
  );
