let statelessComponent = ReasonReact.statelessComponent("MListSubheader");

module Styles = {
  open Css;
  let root = first =>
    style([
      unsafe("lineHeight", (Theme.space(2) |> string_of_int) ++ "px"),
      backgroundColor(Colors.white),
      padding4(
        ~top=px(Theme.space(first ? 0 : 1)),
        ~right=px(0),
        ~bottom=px(Theme.space(1)),
        ~left=px(0),
      ),
    ]);
  let text = style([fontWeight(`num(400)), textTransform(uppercase)]);
  let hr =
    style([
      width(`percent(50.0)),
      height(px(2)),
      margin4(
        ~top=px(Theme.space(2)),
        ~right=px(0),
        ~bottom=px(0),
        ~left=px(0),
      ),
      border(px(0), none, transparent),
      unsafe("backgroundImage", Colors.uGradientOrange),
    ]);
};

let make = (~first=false, children) => {
  ...statelessComponent,
  render: _self =>
    <MaterialUi.ListSubheader className={Styles.root(first)}>
      <span className=Styles.text> ...children </span>
      <hr className=Styles.hr />
    </MaterialUi.ListSubheader>,
};
