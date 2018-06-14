include ViewCommon;

let component = ReasonReact.statelessComponent("VentureInfoBox");

module Styles = {
  open Css;
  let infoBox =
    style([
      border(px(2), solid, Colors.black),
      padding4(
        ~top=px(0),
        ~right=px(Theme.space(4)),
        ~left=px(Theme.space(4)),
        ~bottom=px(Theme.space(4)),
      ),
    ]);
};

let make = _children => {
  ...component,
  render: _self =>
    <div className=Styles.infoBox>
      <MTypography gutterTop=true gutterBottom=true variant=`Title>
        ("What can you do with a venture?" |> text)
      </MTypography>
      <MTypography gutterBottom=true variant=`Body2>
        (
          {js|
                 • Your Venture can receive money from different sources, such as customers, clients, and investors
                |js}
          |> text
        )
      </MTypography>
      <MTypography gutterBottom=true variant=`Body2>
        (
          {js|
                 • Every Partner of the Venture has full transparency of income and payouts
                |js}
          |> text
        )
      </MTypography>
      <MTypography gutterBottom=true variant=`Body2>
        (
          {js|
                 • The team decides the Policies by which payouts take place
                |js}
          |> text
        )
      </MTypography>
    </div>,
};
