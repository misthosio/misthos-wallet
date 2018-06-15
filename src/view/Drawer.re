include ViewCommon;

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
                <LinkButton fullWidth=true route=CreateVenture>
                  ("Create a Venture" |> text)
                </LinkButton>
                <div className=Css.(style([flex(100)])) />
                <MButton
                  gutterTop=false
                  variant=Flat
                  color=`Inherit
                  href=(environment.webDomain ++ "/frequently_asked_questions")>
                  "frequently asked questions"
                </MButton>
                <MButton
                  gutterTop=false
                  variant=Flat
                  color=`Inherit
                  href="mailto:contact@misthos.io">
                  "Contact us"
                </MButton>
                <MButton
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
