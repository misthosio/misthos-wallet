let component = ReasonReact.statelessComponent("Home");

let make = (~data, _children) => {
  ...component,
  render: _self => <button> (ReasonReact.stringToElement("Hello!")) </button>,
};
