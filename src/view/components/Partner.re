include ViewCommon;

open PrimitiveTypes;

let component = ReasonReact.statelessComponent("Partner");

[@bs.module] external avatar : string = "../../assets/img/avatar-bg.svg";

module Styles = {
  open Css;
  let lenght = Theme.space(8);
  let avatar =
    style([
      backgroundImage(url(avatar)),
      backgroundSize(`size((px(lenght), px(lenght)))),
      width(px(lenght)),
      height(px(lenght)),
      fontSize(px(36)),
      lineHeight(1.0),
      fontWeight(600),
    ]);
};

let make = (~partnerId: userId, ~name=?, ~button=?, _children) => {
  ...component,
  render: _self => {
    let userId = partnerId |> UserId.toString;
    let (primary, secondary) =
      switch (name) {
      | Some(name) => (name |> text, Some(userId |> text))
      | None => (userId |> text, None)
      };
    MaterialUi.(
      <ListItem disableGutters=true>
        <Avatar className=Styles.avatar>
          (userId.[0] |> String.make(1) |> String.uppercase |> text)
        </Avatar>
        <ListItemText primary ?secondary />
        (
          switch (button) {
          | None => ReasonReact.null
          | Some(action) =>
            <ListItemSecondaryAction> action </ListItemSecondaryAction>
          }
        )
      </ListItem>
    );
  },
};
