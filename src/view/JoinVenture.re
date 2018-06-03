include ViewCommon;

let component = ReasonReact.statelessComponent("JoinVenture");

let make = (~cmdStatus, _children) => {
  ...component,
  render: (_) =>
    <Grid
      title1=("Joining Venture" |> text)
      area3={<CommandExecutor.Status action=JoinVenture cmdStatus />}
    />,
};
