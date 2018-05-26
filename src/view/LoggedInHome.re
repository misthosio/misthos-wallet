include ViewCommon;

let component = ReasonReact.statelessComponent("LoggedInHome");

let make = (~index, _children) => {
  ...component,
  render: (_) =>
    <Body2
      titles=["My Ventures"]
      body1=
        <div>
          <VentureList index />
          <div />
          <LinkButton route=CreateVenture>
            ("Create a Venture" |> text)
          </LinkButton>
        </div>
      body2=ReasonReact.null
    />,
};
