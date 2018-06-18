include ViewCommon;

module Styles = {
  open Css;
  let alignStart = style([alignSelf(flexStart)]);
};

let component = ReasonReact.statelessComponent("Drawer");

let make = (~onSignOut, ~index, ~selected=?, _children) => {
  ...component,
  render: _self =>
    <Grid
      title1=("My Ventures" |> text)
      area3={
              let environment = Environment.get();
              <div className=ScrollList.containerStyles>
                <ScrollList> <VentureList ?selected index /> </ScrollList>
                <MButton
                  fullWidth=true onClick=(Router.clickToRoute(CreateVenture))>
                  ("Create a Venture" |> text)
                </MButton>
                <div className=Css.(style([flex(100)])) />
                <MButton
                  className=Styles.alignStart
                  gutterTop=false
                  variant=Flat
                  color=`Inherit
                  href=(environment.webDomain ++ "/frequently_asked_questions")>
                  "frequently asked questions"
                </MButton>
                <MButton
                  className=Styles.alignStart
                  gutterTop=false
                  variant=Flat
                  color=`Inherit
                  href="mailto:contact@misthos.io">
                  "Contact us"
                </MButton>
                <MButton
                  className=Styles.alignStart
                  gutterTop=false
                  gutterBottom=true
                  variant=Flat
                  color=`Inherit
                  onClick=onSignOut>
                  "Sign Out"
                </MButton>
              </div>;
            }
    />,
};
