include ViewCommon;

let component = ReasonReact.statelessComponent("LoadVenture");

let make = (~cmdStatus, _children) => {
  ...component,
  render: _ =>
    <Grid
      title1={"Loading Venture" |> text}
      area3={<CommandExecutor.Status action=LoadVenture cmdStatus />}
    />,
};
