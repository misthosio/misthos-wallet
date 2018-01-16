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
  | ProjectCreated(Project.Index.t, Project.t);

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
      ({send}) =>
        Js.Promise.(
          Project.Index.load()
          |> then_(index => send(IndexLoaded(index)) |> resolve)
          |> ignore
        )
    ),
  reducer: (action, state) =>
    switch action {
    | IndexLoaded(index) =>
      ReasonReact.UpdateWithSideEffects(
        {...state, status: None, index},
        (
          ({send}) =>
            switch index {
            | [p, ..._rest] =>
              Js.Promise.(
                Project.load(~projectId=p.id)
                |> then_(project => send(ProjectLoaded(project)) |> resolve)
                |> ignore
              )
            | _ => ()
            }
        )
      )
    | ProjectLoaded(project) =>
      ReasonReact.Update({...state, status: None, selected: Some(project)})
    | ProjectCreated(index, selected) =>
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
            ({send}) =>
              Js.Promise.(
                Project.load(~projectId=id)
                |> then_(project => send(ProjectLoaded(project)) |> resolve)
                |> ignore
              )
          )
        );
    | AddProject =>
      switch (String.trim(state.newProject)) {
      | "" => ReasonReact.NoUpdate
      | name =>
        ReasonReact.UpdateWithSideEffects(
          {...state, status: CreatingProject(name), newProject: ""},
          (
            ({send}) =>
              Js.Promise.(
                Project.Cmd.Create.exec(session, ~name)
                |> then_(((newIndex, project)) =>
                     send(ProjectCreated(newIndex, project)) |> resolve
                   )
                |> ignore
              )
          )
        )
      }
    },
  render: ({send, state}) => {
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
                 onClick=(e => send(selectProject(e)))>
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
        onChange=(e => send(changeNewProject(e)))
        autoFocus=Js.true_
      />
      <button onClick=(_e => send(AddProject))>
        (ReasonReact.stringToElement("Add"))
      </button>
      project
    </div>;
  }
};
