include ViewCommon;

let component = ReasonReact.statelessComponent("Balance");

module Styles = {
  open Css;
  let balance =
    style([
      fontFamily(Theme.sourceSansPro),
      fontWeight(`num(600)),
      fontSize(px(92)),
      fontStyle(normal),
      lineHeight(`abs(1.0)),
      letterSpacing(px(1)),
      unsafe("fill", "rgba(0, 0, 0, 1)"),
    ]);
  let btc = style([fontWeight(`num(300))]);
  let reserved =
    style([fontSize(px(36)), unsafe("fill", "rgba(0, 0, 0, 0.5)")]);
  let container = style([textAlign(center)]);
};

let make = (~currentSpendable, ~reserved=?, _children) => {
  ...component,
  render: _self => {
    let viewBox =
      switch (reserved) {
      | Some(_) => "0 -92 640 184"
      | None => "0 -77 640 92"
      };
    <svg className=Styles.balance width="100%" viewBox>
      <text>
        {currentSpendable |> BTC.format |> text}
        <tspan className=Styles.btc key="currentSpendable">
          {" BTC" |> text}
        </tspan>
      </text>
      {
        switch (reserved) {
        | Some(reserved) =>
          <text dy="55px" className=Styles.reserved>
            {reserved |> BTC.format |> text}
            <tspan className=Styles.btc key="currentSpendable">
              {" BTC IN RESERVE" |> text}
            </tspan>
          </text>
        | None => ReasonReact.null
        }
      }
    </svg>;
  },
};
