type state = {
  project: Project.t,
  viewModel: ViewModel.t,
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
    viewModel: Project.getViewModel(initialProject),
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
        viewModel: Project.getViewModel(project)
      })
    },
  render: ({reduce, state}) => {
    let members =
      ReasonReact.arrayToElement(
        Array.of_list(
          ViewModel.getMembers(state.viewModel)
          |> List.map((m: ViewModel.Member.t) =>
               <li key=m.blockstackId>
                 (ReasonReact.stringToElement(m.blockstackId))
               </li>
             )
        )
      );
    let candidates =
      ReasonReact.arrayToElement(
        Array.of_list(
          ViewModel.getCandidates(state.viewModel)
          |> List.map((candidate: ViewModel.Candidate.t) =>
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
        (ReasonReact.stringToElement(ViewModel.projectName(initialProject)))
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
