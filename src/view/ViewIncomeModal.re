open Belt;
include ViewCommon;

module ViewData = ViewModel.ViewIncomeView;

module Styles = {
  open Css;
  let link =
    style([color(Colors.black), hover([color(Colors.misthosTeal)])]);
};

let component = ReasonReact.statelessComponent("ViewIncomeModal");

let make = (~viewData: ViewData.t, _children) => {
  let {explorerLink, date, status, txId, amount, addresses}: ViewData.t = viewData;
  {
    ...component,
    render: _self => {
      let txStatus = {
        let (label, status: StatusChip.status) =
          switch (status) {
          | Unconfirmed => ("Unconfirmed", Pending)
          | Confirmed => ("Confirmed", Success)
          };
        <StatusChip label status />;
      };
      let addresses =
        addresses
        |. Set.String.reduce("", (res, address) => res ++ address ++ ", ");
      let addresses =
        addresses
        |> Js.String.slice(~from=0, ~to_=Js.String.length(addresses) - 2);
      <Grid
        title1=("Income Transaction Details" |> text)
        area3={
          <div>
            <MTypography variant=`Body2 gutterBottom=true>
              (
                switch (date) {
                | Some(date) =>
                  "Transaction confirmed on "
                  ++ Js.Date.toDateString(date)
                  ++ " "
                  |> text
                | None => ReasonReact.null
                }
              )
            </MTypography>
            <MTypography variant=`Body2>
              ("Status: " |> text)
              txStatus
            </MTypography>
            <MTypography variant=`Title gutterTop=true>
              ("Income Amount" |> text)
            </MTypography>
            <MTypography variant=`Subheading>
              (BTC.format(amount) ++ " BTC" |> text)
            </MTypography>
            <MTypography variant=`Title gutterTop=true>
              ("Income Address" |> text)
            </MTypography>
            <MTypography variant=`Body2> (addresses |> text) </MTypography>
            <MTypography variant=`Title gutterTop=true>
              ("Transaction ID" |> text)
            </MTypography>
            <MTypography variant=`Body2>
              <a className=Styles.link href=explorerLink target="_blank">
                (txId |> text)
              </a>
            </MTypography>
          </div>
        }
      />;
    },
  };
};
