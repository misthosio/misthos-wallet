include ViewCommon;

let component = ReasonReact.statelessComponent("LoggedInHome");

let make = (~index, _children) => {
  ...component,
  render: _ =>
    <Grid
      title1=("My Ventures" |> text)
      area3={
        <div className=ScrollList.containerStyles>
          <ScrollList> <VentureList index /> </ScrollList>
        </div>
      }
      area4={
        <div>
          <MTypography gutterBottom=true variant=`Title>
            ("CREATE A NEW VENTURE" |> text)
          </MTypography>
          <MTypography variant=`Body2>
            (
              "Set up a new Venture with yourself as the initial Partner. You can add and remove Partners once the Venture is created."
              |> text
            )
          </MTypography>
          <MButton
            fullWidth=true onClick=(Router.clickToRoute(CreateVenture))>
            ("Create a Venture" |> text)
          </MButton>
          <ContactUsShoutOut />
        </div>
      }
    />,
};
