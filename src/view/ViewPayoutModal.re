include ViewCommon;

let component = ReasonReact.statelessComponent("Drawer");

let make = _children => {...component, render: _self => text("hello")};
