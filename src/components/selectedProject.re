type action =
  | None;

type state = {id: string};

let component = ReasonReact.reducerComponent("Projects");

let make = (~project, _children) => {
  ...component,
  initialState: () => (),
  reducer: (action, _state) =>
    switch action {
    | None => ReasonReact.Update()
    },
  render: _self =>
    <div>
      <h2> (ReasonReact.stringToElement(Project.getState(project).name)) </h2>
    </div>
};
