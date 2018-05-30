include ViewCommon;

let component = ReasonReact.statelessComponent("ScrollList");

[@bs.module "react-custom-scrollbars"]
external reactClass : ReasonReact.reactClass = "default";

module Styles = {
  open Css;
  let scroll = maxHeightValue =>
    style([overflowY(auto), height(auto), maxHeight(maxHeightValue)]);
};

[@bs.deriving abstract]
type jsProps = {
  renderView: unit => ReasonReact.reactElement,
  renderThumb: unit => ReasonReact.reactElement,
};

/* let renderView = () => <div className=Styles.scroll />; */
/* let make = children => */
/*   ReasonReact.wrapJsForReason( */
/*     ~reactClass, */
/*     ~props={"autoHeight": true, "autoHide": true, "autoHeightMax": 600}, */
/*     children, */
/*   ); */
let make = (~maxHeight=Css.(`percent(100.0)), children) => {
  ...component,
  render: _self =>
    ReasonReact.createDomElement(
      "div",
      ~props={"className": Styles.scroll(maxHeight)},
      children,
    ),
};
