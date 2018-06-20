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
                 • Your Venture contains a multisig bitcoin wallet that you can share access to by adding and removing Partners
                |js}
          |> text
        )
      </MTypography>
      <MTypography gutterBottom=true variant=`Body2>
        (
          {js|
                 • As a team, you and your Partners can receive income and distribute payouts
                |js}
          |> text
        )
      </MTypography>
      <MTypography gutterBottom=true variant=`Body2>
        (
          {js|
                 •  All decisions withn a Venture are executed when a sufficient team consensus has been achieved
                |js}
          |> text
        )
      </MTypography>
    </div>,
};
