include ViewCommon;

let component = ReasonReact.statelessComponent("Transaction");

[@bs.module] external avatar : string = "../../assets/img/avatar-bg.svg";

type inOut =
  | Income
  | Payout;

module Styles = {
  open Css;
  let primary = style([marginRight(px(-8)), lineHeight(1.19)]);
  let amount = (inOut: inOut) =>
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

type tx =
  | Confirmed(ViewModel.confirmedTx)
  | Unconfirmed(ViewModel.unconfirmedTx);

let make = (~tx: tx, _children) => {
  ...component,
  render: _self => {
    let afmt = amount => BTC.format(amount) ++ " BTC" |> text;
    let dfmt = date => Some(Js.Date.toString(date) |> text);
    let inOut =
      switch (tx) {
      | Unconfirmed(UnconfirmedPayout(_, _))
      | Confirmed(ConfirmedPayout(_, _, _)) => Payout
      | Unconfirmed(UnconfirmedIncome(_, _))
      | Confirmed(ConfirmedIncome(_, _, _)) => Income
      };
    let (primary, secondary, amount) =
      switch (tx) {
      | Unconfirmed(utx) =>
        switch (utx) {
        | UnconfirmedPayout(_, amount) => (
            text("UNCONFIRMED PAYOUT"),
            None,
            afmt(amount),
          )
        | UnconfirmedIncome(_, amount) => (
            text("UNCONFIRMED INCOME"),
            None,
            afmt(amount),
          )
        }
      | Confirmed(ctx) =>
        switch (ctx) {
        | ConfirmedIncome(_, amount, date) => (
            text("INCOME"),
            dfmt(date),
            afmt(amount),
          )
        | ConfirmedPayout(_, amount, date) => (
            text("PAYOUT"),
            dfmt(date),
            afmt(amount),
          )
        }
      };
    MaterialUi.(
      <ListItem dense=true disableGutters=true>
        /* <ListItemIcon> <Avatar> ("F" |> Utils.text) </Avatar> </ListItemIcon> */

          <ListItemText
            primary=
              <div className=Styles.primary>
                primary
                <span className=(Styles.amount(inOut))> amount </span>
              </div>
            ?secondary
          />
        </ListItem>
    );
  },
};
