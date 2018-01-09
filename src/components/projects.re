type status =
  | None
  | LoadingIndex
  | LoadingProject
  | CreatingProject(string);

type state = {
  status,
  selected: option(Project.t),
  index: Project.Index.t,
  newProject: string
};

type action =
  | IndexLoaded(Project.Index.t)
  | ProjectLoaded(Project.t)
  | ChangeNewProject(string)
  | SelectProject(string)
  | AddProject
  | ProjectCreated(Project.t, Project.Index.t);

let component = ReasonReact.reducerComponent("Projects");

let changeNewProject = event =>
  ChangeNewProject(
    ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value
  );

let selectProject = e =>
  SelectProject(ReactDOMRe.domElementToObj(ReactEventRe.Mouse.target(e))##id);

let make = (~session, _children) => {
  ...component,
  initialState: () => {
    newProject: "",
    status: LoadingIndex,
    index: [],
    selected: None
  },
  didMount: _self =>
    ReasonReact.SideEffects(
      ({reduce}) =>
        Js.Promise.(
          Project.Index.load()
          |> then_(index => reduce(() => IndexLoaded(index), ()) |> resolve)
        )
        |> ignore
    ),
  reducer: (action, state) =>
    switch action {
    | IndexLoaded(index) =>
      ReasonReact.UpdateWithSideEffects(
        {...state, status: None, index},
        (
          ({reduce}) =>
            switch index {
            | [p, ..._rest] =>
              Project.load(p.id)
              |> Js.Promise.(
                   then_(project =>
                     reduce(() => ProjectLoaded(project), ()) |> resolve
                   )
                 )
              |> ignore
            | _ => ()
            }
        )
      )
    | ProjectLoaded(project) =>
      ReasonReact.Update({...state, status: None, selected: Some(project)})
    | ProjectCreated(selected, index) =>
      ReasonReact.Update({
        ...state,
        status: None,
        index,
        selected: Some(selected)
      })
    | ChangeNewProject(text) =>
      ReasonReact.Update({...state, newProject: text})
    | SelectProject(id) =>
      Js.log("SelectProject(" ++ id ++ ")");
      let selectedId =
        switch state.selected {
        | Some(project) => Project.getId(project)
        | None => ""
        };
      id == selectedId ?
        ReasonReact.NoUpdate :
        ReasonReact.UpdateWithSideEffects(
          {...state, status: LoadingProject, selected: None},
          (
            ({reduce}) =>
              Project.load(id)
              |> Js.Promise.(
                   then_(project =>
                     reduce(() => ProjectLoaded(project), ()) |> resolve
                   )
                 )
              |> ignore
          )
        );
    | AddProject =>
      switch (String.trim(state.newProject)) {
      | "" => ReasonReact.NoUpdate
      | name =>
        ReasonReact.UpdateWithSideEffects(
          {...state, status: CreatingProject(name), newProject: ""},
          (
            ({reduce}) =>
              Project.create(session, name)
              |> Js.Promise.(
                   then_(((project, newIndex)) =>
                     reduce(() => ProjectCreated(project, newIndex), ())
                     |> resolve
                   )
                 )
              |> ignore
          )
        )
      }
    },
  render: ({reduce, state}) => {
    let selectedId =
      switch (state.status, state.selected) {
      | (CreatingProject(_), _) => "new"
      | (_, Some(project)) => Project.getId(project)
      | _ => ""
      };
    let projectList =
      ReasonReact.arrayToElement(
        Array.of_list(
          Project.Index.(
            switch state.status {
            | LoadingIndex => []
            | CreatingProject(newProject) => [
                (newProject, "new"),
                ...state.index |> List.map(({name, id}) => (name, id))
              ]
            | _ => state.index |> List.map(({name, id}) => (name, id))
            }
          )
          |> List.map(((name, id)) =>
               <li
                 key=id
                 id
                 className=(id == selectedId ? "selected" : "")
                 onClick=(reduce(e => selectProject(e)))>
                 (ReasonReact.stringToElement(name))
               </li>
             )
        )
      );
    let status =
      switch state.status {
      | LoadingIndex => ReasonReact.stringToElement("Loading Index")
      | CreatingProject(newProject) =>
        ReasonReact.stringToElement("Creating project '" ++ newProject ++ "'")
      | _ => ReasonReact.stringToElement("projects:")
      };
    let project =
      switch state.selected {
      | Some(project) => <SelectedProject project session />
      | None => <div> (ReasonReact.stringToElement("Loading Project")) </div>
      };
    <div>
      <h2> status </h2>
      <ul> projectList </ul>
      <input
        placeholder="Create new Project"
        value=state.newProject
        onChange=(reduce(changeNewProject))
        autoFocus=Js.true_
      />
      <button onClick=(reduce((_) => AddProject))>
        (ReasonReact.stringToElement("Add"))
      </button>
      project
    </div>;
  }
};
