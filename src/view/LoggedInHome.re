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
          <br />
          <MTypography variant=`Body2>
            (
              {js|
                 Set up a new Venture with yourself as the initial Partner.
                 You can add and remove Partners once the Venture is created.
                |js}
              |> text
            )
          </MTypography>
          <MButton
            fullWidth=true onClick=(Router.clickToRoute(CreateVenture))>
            ("Create a Venture" |> text)
          </MButton>
        </div>
      }
    />,
};
