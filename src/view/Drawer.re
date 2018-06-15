include ViewCommon;

let component = ReasonReact.statelessComponent("Drawer");

let make = (~onSignOut, ~index, ~selected=?, _children) => {
  ...component,
  render: _self =>
    <Grid
      title1=("My Ventures" |> text)
      area3={
        <div className=ScrollList.containerStyles>
          <ScrollList> <VentureList ?selected index /> </ScrollList>
          <LinkButton fullWidth=true route=CreateVenture>
            ("Create a Venture" |> text)
          </LinkButton>
          <div className=Css.(style([flex(100)])) />
          <MButton
            gutterBottom=true variant=Flat color=`Inherit onClick=onSignOut>
            "Sign Out"
          </MButton>
        </div>
      }
    />,
};
