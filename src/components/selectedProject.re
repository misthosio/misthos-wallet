open Project;

type state = {
  project: Project.t,
  viewModel: ViewModel.t,
  candidateId: string,
  worker: ref(Worker.t)
};

type action =
  | ChangeNewMemberId(string)
  | UpdateProject(Project.t)
  | SuggestCandidate
  | WorkerMessage(Worker.Message.receive);

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
    candidateId: "",
    worker: ref(Worker.make(~onMessage=Js.log))
  },
  didMount: ({send, state}) => {
    Worker.terminate(state.worker^);
    let worker =
      ref(Worker.make(~onMessage=message => send(WorkerMessage(message))));
    Js.Promise.(
      Project.getMemberHistoryUrls(session, initialProject)
      |> then_(urls =>
           Worker.Message.RegularlyFetch(urls)
           |> Worker.postMessage(worker^)
           |> resolve
         )
    )
    |> ignore;
    ReasonReact.Update({...state, worker});
  },
  reducer: (action, state) =>
    switch action {
    | WorkerMessage(Fetched(eventLogs)) =>
      Js.log("Received event logs from worker");
      Js.log(eventLogs);
      ReasonReact.NoUpdate;
    | ChangeNewMemberId(text) =>
      ReasonReact.Update({...state, candidateId: text})
    | SuggestCandidate =>
      switch (String.trim(state.candidateId)) {
      | "" => ReasonReact.NoUpdate
      | candidateId =>
        ReasonReact.SideEffects(
          (
            ({send}) =>
              Js.Promise.(
                Cmd.SuggestCandidate.(
                  state.project
                  |> exec(session, ~candidateId)
                  |> then_(result =>
                       (
                         switch result {
                         | Ok(project) => send(UpdateProject(project))
                         | NoUserInfo => Js.log("NoUserInfo")
                         }
                       )
                       |> resolve
                     )
                  |> ignore
                )
              )
          )
        )
      }
    | UpdateProject(project) =>
      Js.Promise.(
        Project.getMemberHistoryUrls(session, project)
        |> then_(urls =>
             Worker.Message.RegularlyFetch(urls)
             |> Worker.postMessage(state.worker^)
             |> resolve
           )
      )
      |> ignore;
      ReasonReact.Update({
        ...state,
        project,
        viewModel: Project.getViewModel(project)
      });
    },
  render: ({send, state}) => {
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
        (ReasonReact.stringToElement(ViewModel.projectName(state.viewModel)))
      </h2>
      (ReasonReact.stringToElement("Members:"))
      <ul> members </ul>
      (ReasonReact.stringToElement("Candidates:"))
      <ul> candidates </ul>
      <input
        placeholder="BlockstackId"
        value=state.candidateId
        onChange=(e => send(changeNewMemberId(e)))
        autoFocus=Js.false_
      />
      <button onClick=(_e => send(SuggestCandidate))>
        (ReasonReact.stringToElement("Suggest Candidate"))
      </button>
    </div>;
  }
};
