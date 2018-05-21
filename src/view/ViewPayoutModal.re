include ViewCommon;

module ViewData = ViewModel.ViewPayoutView;

let component = ReasonReact.statelessComponent("Drawer");

let make = (~viewData, _children) => {
  ...component,
  render: _self => <div> ("hello" |> text) </div>,
};
