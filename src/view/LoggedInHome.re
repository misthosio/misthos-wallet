include ViewCommon;

let component = ReasonReact.statelessComponent("LoggedInHome");

let make = (~index, _children) => {
  ...component,
  render: (_) =>
    <Grid
      title1=("My Ventures" |> text)
      area3=
        <div>
          <VentureList index />
          <div />
          <LinkButton route=CreateVenture>
            ("Create a Venture" |> text)
          </LinkButton>
        </div>
    />,
};
