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
          <VentureInfoBox />
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
          <LinkButton fullWidth=true route=CreateVenture>
            ("Create a Venture" |> text)
          </LinkButton>
        </div>
      }
    />,
};
