type state =
  | NotLoaded
  | Loaded(list(Project.t));

type action =
  | ProjectsLoaded(list(Project.t));

let component = ReasonReact.reducerComponent("Projects");

let make = (_children) => {
  ...component,
  initialState: () => NotLoaded,
  reducer: (action, _state) =>
    switch action {
    | ProjectsLoaded(projects) => ReasonReact.Update(Loaded(projects))
    },
  didMount: ({reduce}) => {
    Js.Promise.(
      Project.listAll()
      |> then_((projects) => reduce(() => ProjectsLoaded(projects), ()) |> resolve)
    )
    |> ignore;
    ReasonReact.NoUpdate
  },
  render: ({state}) =>
    <div>
      (
        switch state {
        | NotLoaded => ReasonReact.stringToElement("NotLoaded")
        | Loaded(projects) => ReasonReact.stringToElement("loaded")
        }
      )
    </div>
};
