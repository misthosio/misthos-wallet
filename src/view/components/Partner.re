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

let make = (~partner: ViewModel.partner, _children) => {
  ...component,
  render: _self => {
    let userId = partner.userId |> UserId.toString;
    let (primary, secondary) =
      switch (partner.name) {
      | Some(name) => (name |> Utils.text, Some(userId |> Utils.text))
      | None => (userId |> Utils.text, None)
      };
    MaterialUi.(
      <ListItem key=userId disableGutters=true>
        <Avatar className=Styles.avatar>
          (userId.[0] |> String.make(1) |> String.uppercase |> Utils.text)
        </Avatar>
        <ListItemText primary ?secondary />
      </ListItem>
    );
  },
};
