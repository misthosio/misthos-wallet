include ViewCommon;

module ViewData = ViewModel.SelectedVentureView;

type icon =
  | Plus
  | Minus
  | ArrowUp;

let component = ReasonReact.statelessComponent("Transaction");

[@bs.module] external plus : string = "../../assets/img/plus-circle.svg";

[@bs.module] external minus : string = "../../assets/img/minus-circle.svg";

[@bs.module]
external arrowUp : string = "../../assets/img/arrow-up-circle.svg";

[@bs.module] external arrowRight : string = "../../assets/img/arrow-right.svg";

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

let make = (~icon: icon, ~onClick, ~text, _children) => {
  ...component,
  render: _self => {
    let (src, alt) =
      switch (icon) {
      | Plus => (plus, "plus-icon")
      | Minus => (minus, "minus-icon")
      | ArrowUp => (arrowUp, "arrow-up-icon")
      };
    MaterialUi.(
      <ListItem className=Styles.alert dense=true button=true onClick>
        <Avatar className=Styles.icon> <img src alt /> </Avatar>
        <ListItemText primary=text />
        <ListItemSecondaryAction>
          <IconButton onClick>
            <img src=arrowRight alt="arrow-right-icon" />
          </IconButton>
        </ListItemSecondaryAction>
      </ListItem>
    );
  },
};
