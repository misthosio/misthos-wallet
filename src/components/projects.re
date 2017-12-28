open Project;

type state = {
  newProject: string,
  loading: bool,
  projects: list(Project.t)
};

type action =
  | ProjectsLoaded(list(Project.t))
  | ChangeNewProject(string);

let component = ReasonReact.reducerComponent("Projects");

let changeNewProject = (event) =>
  ChangeNewProject(ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value);

let make = (_children) => {
  ...component,
  initialState: () => {newProject: "", loading: true, projects: []},
  reducer: (action, state) =>
    switch action {
    | ProjectsLoaded(projects) => ReasonReact.Update({...state, loading: false, projects})
    | ChangeNewProject(text) => ReasonReact.Update({...state, newProject: text})
    },
  didMount: ({reduce}) => {
    Js.Promise.(
      Project.listAll()
      |> then_((projects) => reduce(() => ProjectsLoaded(projects), ()) |> resolve)
    )
    |> ignore;
    ReasonReact.NoUpdate
  },
  render: ({reduce, state}) => {
    let projectList =
      ReasonReact.arrayToElement(
        Array.of_list(
          state.loading ?
            [] :
            state.projects |> List.map(({name}) => <ul> (ReasonReact.stringToElement(name)) </ul>)
        )
      );
    <div>
      projectList
      <input
        placeholder="Create new Project"
        value=state.newProject
        /* onKeyDown=(reduce(newProjectKeyDown)) */
        onChange=(reduce(changeNewProject))
        autoFocus=Js.true_
      />
    </div>
  }
};
