open Venture;

let text = ReasonReact.stringToElement;

type state = {
  venture: Venture.t,
  viewModel: ViewModel.t,
  prospectId: string,
  worker: ref(Worker.t)
};

type action =
  | WorkerMessage(Worker.Message.receive)
  | ChangeNewPartnerId(string)
  | UpdateVenture(Venture.t)
  | SuggestProspect
  | ApproveProspect(string)
  | SubmitContribution(int, int, string, string)
  | ApproveContribution(string);

let changeNewPartnerId = event =>
  ChangeNewPartnerId(
    ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value
  );

let submitContribution =
    (send, ~amountInteger, ~amountFraction, ~currency, ~description) =>
  send(
    SubmitContribution(amountInteger, amountFraction, currency, description)
  );

let component = ReasonReact.reducerComponent("SelectedVenture");

let make = (~venture as initialVenture, ~session, _children) => {
  ...component,
  initialState: () => {
    venture: initialVenture,
    viewModel: Venture.getViewModel(initialVenture),
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
          Venture.getPartnerHistoryUrls(session, initialVenture)
          |> then_(urls =>
               Worker.Message.RegularlyFetch(
                 urls,
                 Venture.getSummary(initialVenture)
               )
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
      ReasonReact.SideEffects(
        (
          ({send, state}) =>
            Js.Promise.(
              Cmd.Synchronize.(
                state.venture
                |> exec(eventLogs)
                |> then_(
                     fun
                     | Ok(venture) => send(UpdateVenture(venture)) |> resolve
                     | Error(venture, _item, _result) => {
                         Js.log("An error occured while synchronizing");
                         send(UpdateVenture(venture)) |> resolve;
                       }
                   )
                |> ignore
              )
            )
        )
      )
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
                  state.venture
                  |> exec(session, ~prospectId)
                  |> then_(result =>
                       (
                         switch result {
                         | Ok(venture) => send(UpdateVenture(venture))
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
    | ApproveProspect(prospectId) =>
      ReasonReact.SideEffects(
        (
          ({send}) =>
            Js.Promise.(
              Cmd.ApproveProspect.(
                state.venture
                |> exec(session, ~prospectId)
                |> then_(result =>
                     (
                       switch result {
                       | Ok(venture) => send(UpdateVenture(venture))
                       }
                     )
                     |> resolve
                   )
                |> ignore
              )
            )
        )
      )
    | SubmitContribution(amountInteger, amountFraction, currency, description) =>
      ReasonReact.SideEffects(
        (
          ({send}) =>
            Js.Promise.(
              Cmd.SubmitContribution.(
                state.venture
                |> exec(
                     session,
                     ~amountInteger,
                     ~amountFraction,
                     ~currency,
                     ~description
                   )
                |> then_(result =>
                     (
                       switch result {
                       | Ok(venture) => send(UpdateVenture(venture))
                       }
                     )
                     |> resolve
                   )
                |> ignore
              )
            )
        )
      )
    | ApproveContribution(processId) =>
      ReasonReact.SideEffects(
        (
          ({send}) =>
            Js.Promise.(
              Cmd.ApproveContribution.(
                state.venture
                |> exec(session, ~processId)
                |> then_(result =>
                     (
                       switch result {
                       | Ok(venture) => send(UpdateVenture(venture))
                       }
                     )
                     |> resolve
                   )
                |> ignore
              )
            )
        )
      )
    | UpdateVenture(venture) =>
      Js.Promise.(
        Venture.getPartnerHistoryUrls(session, venture)
        |> then_(urls =>
             Worker.Message.RegularlyFetch(urls, Venture.getSummary(venture))
             |> Worker.postMessage(state.worker^)
             |> resolve
           )
        |> ignore
      );
      ReasonReact.Update({
        ...state,
        venture,
        viewModel: Venture.getViewModel(venture)
      });
    },
  render: ({send, state}) => {
    let partners =
      ReasonReact.arrayToElement(
        Array.of_list(
          ViewModel.getPartners(state.viewModel)
          |> List.map((m: ViewModel.partner) =>
               <li key=m.blockstackId> (text(m.blockstackId)) </li>
             )
        )
      );
    let prospects =
      ReasonReact.arrayToElement(
        Array.of_list(
          ViewModel.getProspects(state.viewModel)
          |> List.map((prospect: ViewModel.prospect) =>
               <li key=prospect.blockstackId>
                 (
                   text(
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
                 <button
                   onClick=(_e => send(ApproveProspect(prospect.blockstackId)))>
                   (text("Approve Prospect"))
                 </button>
               </li>
             )
        )
      );
    let contributions =
      ReasonReact.arrayToElement(
        Array.of_list(
          ViewModel.getContributions(state.viewModel)
          |> List.filter((contribution: ViewModel.contribution) =>
               contribution.accepted == true
             )
          |> List.map((contribution: ViewModel.contribution) =>
               <li key=contribution.processId>
                 (
                   text(
                     "'"
                     ++ contribution.description
                     ++ "' approved by: "
                     ++ List.fold_left(
                          (state, partnerId) => state ++ partnerId ++ " ",
                          "",
                          contribution.supporters
                        )
                   )
                 )
               </li>
             )
        )
      );
    let contributionProcesses =
      ReasonReact.arrayToElement(
        Array.of_list(
          ViewModel.getContributions(state.viewModel)
          |> List.filter((contribution: ViewModel.contribution) =>
               contribution.accepted == false
             )
          |> List.map((contribution: ViewModel.contribution) =>
               <li key=contribution.processId>
                 (
                   text(
                     "'"
                     ++ contribution.description
                     ++ "' approved by: "
                     ++ List.fold_left(
                          (state, partnerId) => state ++ partnerId ++ " ",
                          "",
                          contribution.supporters
                        )
                   )
                 )
                 <button
                   onClick=(
                     _e => send(ApproveContribution(contribution.processId))
                   )>
                   (text("Approve Contribution"))
                 </button>
               </li>
             )
        )
      );
    <div>
      <h2> (text(ViewModel.ventureName(state.viewModel))) </h2>
      (text("Contributions:"))
      <ul> contributions </ul>
      (text("Pending acceptance:"))
      <ul> contributionProcesses </ul>
      <ContributionInput submit=(submitContribution(send)) />
      (text("Partners:"))
      <ul> partners </ul>
      (text("Prospects:"))
      <ul> prospects </ul>
      <input
        placeholder="BlockstackId"
        value=state.prospectId
        onChange=(e => send(changeNewPartnerId(e)))
        autoFocus=Js.false_
      />
      <button onClick=(_e => send(SuggestProspect))>
        (text("Suggest Prospect"))
      </button>
    </div>;
  }
};
