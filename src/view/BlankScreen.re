include ViewCommon;

let component = ReasonReact.statelessComponent("JoinVenture");

let make = (~text as notice, _children) => {
  ...component,
  render: (_) =>
    <Body2 titles=[""] body1=(text(notice)) body2=ReasonReact.null />,
};
