include ViewCommon;

let component = ReasonReact.statelessComponent("BlankScreen");

let make = (~text as notice, _children) => {
  ...component,
  render: (_) => <Spinner text=notice />,
};
