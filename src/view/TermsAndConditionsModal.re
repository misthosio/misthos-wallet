include ViewCommon;

let component = ReasonReact.statelessComponent("ViewIncomeModal");
let make = _children => {
  ...component,
  render: _ => <div> ("dummy" |> text) </div>,
};
