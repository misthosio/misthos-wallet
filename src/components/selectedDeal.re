open Deal;

type state = {
  project: Deal.t,
  viewModel: ViewModel.t,
  prospectId: string,
  worker: ref(Worker.t)
};

type action =
  | ChangeNewPartnerId(string)
  | UpdateDeal(Deal.t)
  | SuggestProspect
  | WorkerMessage(Worker.Message.receive);

let changeNewPartnerId = event =>
  ChangeNewPartnerId(
    ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value
  );

let component = ReasonReact.reducerComponent("SelectedDeal");

let make = (~project as initialDeal, ~session, _children) => {
  ...component,
  initialState: () => {
    project: initialDeal,
    viewModel: Deal.getViewModel(initialDeal),
    prospectId: "",
    worker: ref(Worker.make(~onMessage=Js.log))
  },
  subscriptions: ({send, state}) => [
    Sub(
      () => {
        Worker.terminate(state.worker^);
        let worker =
          Worker.make(~onMessage=message => send(WorkerMessage(message)));
        Js.Promise.(
          Deal.getPartnerHistoryUrls(session, initialDeal)
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
    | ChangeNewPartnerId(text) =>
      ReasonReact.Update({...state, prospectId: text})
    | SuggestProspect =>
      switch (String.trim(state.prospectId)) {
      | "" => ReasonReact.NoUpdate
      | prospectId =>
        ReasonReact.SideEffects(
          (
            ({send}) =>
              Js.Promise.(
                Cmd.SuggestProspect.(
                  state.project
                  |> exec(session, ~prospectId)
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
        Deal.getPartnerHistoryUrls(session, project)
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
    let partners =
      ReasonReact.arrayToElement(
        Array.of_list(
          ViewModel.getPartners(state.viewModel)
          |> List.map((m: ViewModel.Partner.t) =>
               <li key=m.blockstackId>
                 (ReasonReact.stringToElement(m.blockstackId))
               </li>
             )
        )
      );
    let prospects =
      ReasonReact.arrayToElement(
        Array.of_list(
          ViewModel.getProspects(state.viewModel)
          |> List.map((prospect: ViewModel.Prospect.t) =>
               <li key=prospect.blockstackId>
                 (
                   ReasonReact.stringToElement(
                     "'"
                     ++ prospect.blockstackId
                     ++ "' approved by: "
                     ++ List.fold_left(
                          (state, partnerId) => state ++ partnerId ++ " ",
                          "",
                          prospect.approvedBy
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
      (ReasonReact.stringToElement("Partners:"))
      <ul> partners </ul>
      (ReasonReact.stringToElement("Prospects:"))
      <ul> prospects </ul>
      <input
        placeholder="BlockstackId"
        value=state.prospectId
        onChange=(e => send(changeNewPartnerId(e)))
        autoFocus=Js.false_
      />
      <button onClick=(_e => send(SuggestProspect))>
        (ReasonReact.stringToElement("Suggest Prospect"))
      </button>
    </div>;
  }
};
