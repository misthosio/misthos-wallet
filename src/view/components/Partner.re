include ViewCommon;

open PrimitiveTypes;

let component = ReasonReact.statelessComponent("Partner");

module Styles = {
  open Css;
  let lenght = Theme.space(6);
  let avatar =
    style([
      backgroundImage(url(Icons.avatar |> Icons.asDataUrl)),
      backgroundSize(`size((px(lenght), px(lenght)))),
      width(px(lenght)),
      height(px(lenght)),
      fontSize(px(24)),
      lineHeight(1.0),
      fontWeight(600),
    ]);
  let primary =
    style([
      fontFamily(Theme.oswald),
      fontSize(px(16)),
      fontWeight(600),
      unsafe("letterSpacing", "0.7px"),
      textTransform(uppercase),
      whiteSpace(nowrap),
      overflow(hidden),
      textOverflow(ellipsis),
    ]);
  let secondary =
    style([
      fontFamily(Theme.sourceSansPro),
      fontSize(px(16)),
      fontWeight(300),
      unsafe("letterSpacing", "0.5px"),
      color(rgba(0, 0, 0, 0.87)),
    ]);
  let secondaryAction = status =>
    switch (status) {
    | Some(_) => style([paddingRight(px(Theme.space(12)))])
    | None => style([paddingRight(px(Theme.space(4)))])
    };
};

let make =
    (~partnerId: userId, ~name=?, ~button=?, ~status=?, ~onClick=?, _children) => {
  ...component,
  render: _self => {
    let userId = partnerId |> UserId.toString;
    let (primary, secondary) =
      switch (name) {
      | Some(name) => (name |> text, Some(userId |> text))
      | None => (userId |> text, None)
      };
    MaterialUi.(
      <ListItem
        classes=[SecondaryAction(Styles.secondaryAction(status))]
        disableGutters=true
        ?onClick>
        <Avatar className=Styles.avatar>
          (userId.[0] |> String.make(1) |> String.uppercase |> text)
        </Avatar>
        <ListItemText
          classes=[Primary(Styles.primary), Secondary(Styles.secondary)]
          primary
          ?secondary
        />
        (
          switch (button, status) {
          | (None, None) => ReasonReact.null
          | (Some(action), _) =>
            <ListItemSecondaryAction> action </ListItemSecondaryAction>
          | (_, Some(action)) =>
            <ListItemSecondaryAction> action </ListItemSecondaryAction>
          }
        )
      </ListItem>
    );
  },
};
