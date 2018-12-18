include ViewCommon;

let component = ReasonReact.statelessComponent("ContactUsShoutOut");

module Styles = {
  open Css;
  let root = gutterBottom =>
    style([
      margin4(
        ~top=px(Theme.space(2)),
        ~bottom=px(gutterBottom ? Theme.space(2) : 0),
        ~left=px(0),
        ~right=px(0),
      ),
    ]);
  let link =
    style([
      color(Colors.misthosTeal),
      unsafe("textDecorationColor", Colors.uMisthosTeal),
      hover([color(Colors.black)]),
    ]);
};

let make = (~gutterBottom=true, _children) => {
  ...component,
  render: _self =>
    <div className={Styles.root(gutterBottom)}>
      <MTypography variant=`Body2>
        <a className=Styles.link href="mailto:contact@misthos.io">
          {"Contact us" |> text}
        </a>
        {{js| if you’d like a demo or help setting up your team.|js} |> text}
      </MTypography>
    </div>,
};
