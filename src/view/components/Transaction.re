include ViewCommon;

let component = ReasonReact.statelessComponent("Transaction");

type txType =
  | Income
  | Payout;

module Styles = {
  open Css;
  let root =
    style([
      flex(1),
      padding2(~v=px(0), ~h=px(16)),
      minWidth(px(0)),
      firstChild([paddingLeft(px(16))]),
    ]);
  let divider = style([borderBottom(px(1), `solid, hex("979797"))]);
  let amount = (inOut: txType) =>
    style([
      color(
        switch (inOut) {
        | Income => Colors.misthosTeal
        | Payout => Colors.strongPink
        },
      ),
      Css.float(`right),
    ]);
};

let make =
    (
      ~txType: txType,
      ~primary: string,
      ~amount: BTC.t,
      ~date: option(Js.Date.t),
      ~onClick=?,
      _children,
    ) => {
  ...component,
  render: _self =>
    MaterialUi.(
      <ListItem
        classes=[Divider(Styles.divider)]
        dense=true
        disableGutters=true
        button=(onClick != None)
        ?onClick
        divider=true>
        <ListItemText
          classes=[Root(Styles.root)]
          primary={
            <MTypography variant=`Body2>
              (primary |> String.uppercase |> text)
              <span className=(Styles.amount(txType))>
                (BTC.format(amount) ++ " BTC" |> text)
              </span>
            </MTypography>
          }
          secondary=(
            switch (date) {
            | Some(date) =>
              <MTypography variant=`Body1>
                (Js.Date.toDateString(date) |> text)
              </MTypography>
            | None => ReasonReact.null
            }
          )
        />
      </ListItem>
    ),
};
