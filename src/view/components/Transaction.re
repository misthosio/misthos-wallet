include ViewCommon;

module ViewData = ViewModel.SelectedVentureView;

let component = ReasonReact.statelessComponent("Transaction");

[@bs.module] external avatar : string = "../../assets/img/avatar-bg.svg";

module Styles = {
  open Css;
  let root =
    style([
      flex(1),
      padding2(~v=px(0), ~h=px(16)),
      minWidth(px(0)),
      firstChild([paddingLeft(px(16))]),
    ]);
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
    let dfmt =
      Utils.mapOption(date =>
        <MTypography variant=`Body1>
          (Js.Date.toDateString(date) |> text)
        </MTypography>
      );
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
        <ListItemText
          classes=[Root(Styles.root)]
          primary={
            <MTypography variant=`Body2>
              primary
              <span className=(Styles.amount(tx.txType))> amount </span>
            </MTypography>
          }
          ?secondary
        />
      </ListItem>
    );
  },
};
