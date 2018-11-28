include ViewCommon;

type status =
  | Neutral
  | Pending
  | Failure
  | Success;

let component = ReasonReact.statelessComponent("StatusChip");

module Styles = {
  open Css;
  let chip = status =>
    style([
      backgroundColor(
        switch (status) {
        | Neutral => Colors.grayedOut
        | Pending => rgba(245, 166, 35, 0.2)
        | Failure => rgba(255, 50, 83, 0.2)
        | Success => rgba(2, 162, 180, 0.2)
        },
      ),
      color(
        switch (status) {
        | Neutral => hex("7f7f7f")
        | Pending => Colors.warning
        | Failure => Colors.error
        | Success => Colors.misthosTeal
        },
      ),
      fontFamily(Theme.sourceSansPro),
      fontWeight(`num(400)),
      fontSize(px(12)),
      textTransform(uppercase),
      borderRadius(px(0)),
      minWidth(px(Theme.space(11))),
    ]);
};

let make = (~status: status, ~label: string, _children) => {
  ...component,
  render: _self => {
    let label = text(label);
    <MaterialUi.Chip className={Styles.chip(status)} label />;
  },
};
