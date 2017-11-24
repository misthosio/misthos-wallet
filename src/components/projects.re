type projects =
  | NotLoaded
  | Loaded(string);

type action =
  | ProjectsLoaded(string);

let component = ReasonReact.reducerComponent("Projects");

let make = (_children) => {
  ...component,
  initialState: () => NotLoaded,
  reducer: (action, _state) =>
    switch action {
    | ProjectsLoaded(projects) => ReasonReact.Update(Loaded(projects))
    },
  /* didMount: ({reduce}) => { */
  /*   Js.Promise.( */
  /*     State.loadProjects() */
  /*     |> then_((projects) => reduce(() => ProjectsLoaded(projects), ()) |> resolve) */
  /*   ) */
  /*   |> ignore; */
  /*   ReasonReact.NoUpdate */
  /* }, */
  render: ({state}) =>
    <div
      /* ( */
      /*   switch state { */
      /*   | NotLoaded => ReasonReact.stringToElement("NotLoaded") */
      /*   | Loaded(projects) => ReasonReact.stringToElement(projects) */
      /*   } */
      /* ) */
    />
};
