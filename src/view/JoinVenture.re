include ViewCommon;

let component = ReasonReact.statelessComponent("JoinVenture");

let make = (~cmdStatus, _children) => {
  ...component,
  render: (_) =>
    <Body2
      titles=["Joining Venture"]
      body1={<CommandExecutor.Status action=JoinVenture cmdStatus />}
      body2=ReasonReact.null
    />,
};
