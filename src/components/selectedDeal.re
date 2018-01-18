open Deal;

type state = {
  project: Deal.t,
  viewModel: ViewModel.t,
  candidateId: string,
  worker: ref(Worker.t)
};

type action =
  | ChangeNewMemberId(string)
  | UpdateDeal(Deal.t)
  | SuggestCandidate
  | WorkerMessage(Worker.Message.receive);

let changeNewMemberId = event =>
  ChangeNewMemberId(
    ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value
  );

let component = ReasonReact.reducerComponent("SelectedDeal");

let make = (~project as initialDeal, ~session, _children) => {
  ...component,
  initialState: () => {
    project: initialDeal,
    viewModel: Deal.getViewModel(initialDeal),
    candidateId: "",
    worker: ref(Worker.make(~onMessage=Js.log))
  },
  subscriptions: ({send, state}) => [
    Sub(
      () => {
        Worker.terminate(state.worker^);
        let worker =
          Worker.make(~onMessage=message => send(WorkerMessage(message)));
        Js.Promise.(
          Deal.getMemberHistoryUrls(session, initialDeal)
          |> then_(urls =>
               Worker.Message.RegularlyFetch(urls)
               |> Worker.postMessage(worker)
               |> resolve
             )
          |> ignore
        );
        state.worker := worker;
        worker;
      },
      Worker.terminate
    )
  ],
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
                         | Ok(project) => send(UpdateDeal(project))
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
    | UpdateDeal(project) =>
      Js.Promise.(
        Deal.getMemberHistoryUrls(session, project)
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
        viewModel: Deal.getViewModel(project)
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
