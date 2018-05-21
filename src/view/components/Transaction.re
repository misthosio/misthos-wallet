include ViewCommon;

module ViewData = ViewModel.SelectedVentureView;

let component = ReasonReact.statelessComponent("Transaction");

[@bs.module] external avatar : string = "../../assets/img/avatar-bg.svg";

module Styles = {
  open Css;
  let primary = style([marginRight(px(-8)), lineHeight(1.19)]);
  let amount = (inOut: ViewData.txType) =>
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

let make = (~tx: ViewData.txData, _children) => {
  ...component,
  render: _self => {
    let afmt = amount => BTC.format(amount) ++ " BTC" |> text;
    let dfmt = Utils.mapOption(date => Js.Date.toString(date) |> text);
    let (primary, secondary, amount) = (
      switch (tx.status, tx.txType) {
      | (Unconfirmed, Payout) => text("UNCONFIRMED PAYOUT")
      | (Unconfirmed, Income) => text("UNCONFIRMED INCOME")
      | (Confirmed, Payout) => text("PAYOUT")
      | (Confirmed, Income) => text("INCOME")
      },
      dfmt(tx.date),
      afmt(tx.amount),
    );
    MaterialUi.(
      <ListItem
        dense=true
        disableGutters=true
        button=true
        onClick=(Router.clickToRoute(tx.detailsLink))>
        /* <ListItemIcon> <Avatar> ("F" |> Utils.text) </Avatar> </ListItemIcon> */

          <ListItemText
            primary=
              <div className=Styles.primary>
                primary
                <span className=(Styles.amount(tx.txType))> amount </span>
              </div>
            ?secondary
          />
        </ListItem>
    );
  },
};
