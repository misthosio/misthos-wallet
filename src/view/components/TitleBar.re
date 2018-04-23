let component = ReasonReact.statelessComponent("TitleBar");

module Styles = {
  open Css;
  let title = style([backgroundColor(Colors.black)]);
  let gradient = style([height(px(4)), backgroundImage(Colors.gradient)]);
};

let make = children => {
  ...component,
  render: _self =>
    MaterialUi.(
      <Grid className=Styles.title item=true xs=V12>
        (ReasonReact.arrayToElement(children))
        <div className=Styles.gradient />
      </Grid>
    ),
};
