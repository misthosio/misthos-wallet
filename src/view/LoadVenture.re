include ViewCommon;

let component = ReasonReact.statelessComponent("LoadVenture");

let make = (~cmdStatus, _children) => {
  ...component,
  render: (_) =>
    <Body2
      titles=["Loading Venture"]
      body1={<CommandExecutor.Status action=LoadVenture cmdStatus />}
      body2=ReasonReact.null
    />,
};
