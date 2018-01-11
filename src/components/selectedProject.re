open Project;

type state = {
  project: Project.t,
  viewState: ViewState.t,
  candidateId: string
};

type action =
  | ChangeNewMemberId(string)
  | UpdateProject(Project.t)
  | SuggestCandidate;

let changeNewMemberId = event =>
  ChangeNewMemberId(
    ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value
  );

let component = ReasonReact.reducerComponent("SelectedProject");

let make = (~project as initialProject, ~session, _children) => {
  ...component,
  initialState: () => {
    project: initialProject,
    viewState: Project.getViewState(initialProject),
    candidateId: ""
  },
  reducer: (action, state) =>
    switch action {
    | ChangeNewMemberId(text) =>
      ReasonReact.Update({...state, candidateId: text})
    | SuggestCandidate =>
      switch (String.trim(state.candidateId)) {
      | "" => ReasonReact.NoUpdate
      | candidateId =>
        ReasonReact.SideEffects(
          (
            ({reduce}) =>
              state.project
              |> Project.Command.suggestCandidate(session, candidateId)
              |> Js.Promise.(
                   then_(project =>
                     reduce(() => UpdateProject(project), ()) |> resolve
                   )
                 )
              |> ignore
          )
        )
      }
    | UpdateProject(project) =>
      ReasonReact.Update({
        ...state,
        project,
        viewState: Project.getViewState(project)
      })
    },
  render: ({reduce, state}) => {
    let members =
      ReasonReact.arrayToElement(
        Array.of_list(
          ViewState.getMembers(state.viewState)
          |> List.map((m: ViewState.Member.t) =>
               <li key=m.blockstackId>
                 (ReasonReact.stringToElement(m.blockstackId))
               </li>
             )
        )
      );
    let candidates =
      ReasonReact.arrayToElement(
        Array.of_list(
          ViewState.getCandidates(state.viewState)
          |> List.map((candidate: ViewState.Candidate.t) =>
               <li key=candidate.blockstackId>
                 (
                   ReasonReact.stringToElement(
                     "'"
                     ++ candidate.blockstackId
                     ++ "' approved by: "
                     ++ List.fold_left(
                          (state, memberId) => state ++ memberId ++ " ",
                          "",
                          candidate.approvedBy
                        )
                   )
                 )
               </li>
             )
        )
      );
    <div>
      <h2>
        (ReasonReact.stringToElement(ViewState.projectName(initialProject)))
      </h2>
      (ReasonReact.stringToElement("Members:"))
      <ul> members </ul>
      (ReasonReact.stringToElement("Candidates:"))
      <ul> candidates </ul>
      <input
        placeholder="BlockstackId"
        value=state.candidateId
        onChange=(reduce(changeNewMemberId))
        autoFocus=Js.false_
      />
      <button onClick=(reduce((_) => SuggestCandidate))>
        (ReasonReact.stringToElement("Suggest Candidate"))
      </button>
    </div>;
  }
};
