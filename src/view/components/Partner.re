include ViewCommon;

open PrimitiveTypes;

let component = ReasonReact.statelessComponent("Partner");

module Styles = {
  open Css;
  let lenght = Theme.space(8);
  let avatar =
    style([
      backgroundColor(`transparent),
      width(px(lenght)),
      height(px(lenght)),
    ]);
  let primary =
    style([
      fontFamily(Theme.oswald),
      fontSize(px(18)),
      fontWeight(600),
      unsafe("letterSpacing", "0.7px"),
      textDecoration(underline),
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
  let iconText =
    style([
      fontFamily(Theme.sourceSansPro),
      fontSize(px(36)),
      lineHeight(1.0),
      fontWeight(600),
    ]);
};

let avatar = (letter: char) =>
  <svg
    xmlns="http://www.w3.org/2000/svg"
    width="70"
    height="70"
    viewBox="0 0 70 70">
    <defs>
      <linearGradient id="b" x1="-1.146%" y1="71.336%" y2="0%">
        <stop offset="0%" stopColor="#59F7F0" />
        <stop offset="28.22%" stopColor="#02A2B4" />
        <stop offset="56.765%" stopColor="#067781" />
        <stop offset="79.931%" stopColor="#FF006D" />
        <stop offset="100%" stopColor="#F65E25" />
      </linearGradient>
    </defs>
    <g fill="none" fillRule="evenodd" transform="translate(6 6)">
      <ellipse
        fill="#000"
        fillOpacity=".98"
        cx="29"
        cy="28.642"
        rx="29"
        ry="28.642"
      />
      <ellipse
        cx="29"
        cy="28.642"
        stroke="url(#b)"
        strokeWidth="6"
        rx="32"
        ry="31.642"
      />
      <ellipse
        cx="29"
        cy="28.642"
        stroke="#FFF"
        strokeWidth="4"
        rx="31"
        ry="30.642"
      />
    </g>
    <text
      fill="#FFF"
      fontSize="36px"
      fontWeight="600"
      x="50%"
      y="50%"
      textAnchor="middle"
      alignmentBaseline="middle"
      fontFamily=Theme.sourceSansPro>
      (letter |> String.make(1) |> String.uppercase |> text)
    </text>
  </svg>;

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
        <Avatar className=Styles.avatar> (userId.[0] |> avatar) </Avatar>
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
