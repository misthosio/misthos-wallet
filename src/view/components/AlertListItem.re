include ViewCommon;

type icon =
  | Plus
  | Minus
  | ArrowUp;

let component = ReasonReact.statelessComponent("AlertListItem");

module Styles = {
  open Css;
  let alert =
    style([
      borderStyle(solid),
      borderWidth(px(2)),
      unsafe("borderImageSlice", "1"),
      unsafe("borderImageSource", Colors.uGradientAqua),
      marginBottom(px(Theme.space(1))),
    ]);
  let icon =
    style([unsafe("width", "min-content"), backgroundColor(`transparent)]);
};

let make = (~icon: icon, ~onClick, ~primary, ~secondary=?, _children) => {
  ...component,
  render: _self => {
    let icon =
      switch (icon) {
      | Plus => Icons.plusCircle
      | Minus => Icons.minusCircle
      | ArrowUp => Icons.arrowUpCircle
      };
    MaterialUi.(
      <ListItem className=Styles.alert dense=true button=true onClick>
        <Avatar className=Styles.icon> icon </Avatar>
        <ListItemText primary ?secondary />
        <ListItemSecondaryAction>
          <IconButton onClick> Icons.arrowRight </IconButton>
        </ListItemSecondaryAction>
      </ListItem>
    );
  },
};
