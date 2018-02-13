open Venture;

open PrimitiveTypes;

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
  | ProposePartner
  | EndorsePartner(ProcessId.t)
  | ProposePartnerLabel(UserId.t, LabelId.t)
  | EndorsePartnerLabel(ProcessId.t)
  | ProposeContribution(int, int, string, string)
  | EndorseContribution(ProcessId.t);

let changeNewPartnerId = event =>
  ChangeNewPartnerId(
    ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value
  );

let proposePartnerLabel = (send, userId, ~labelId) =>
  send(ProposePartnerLabel(userId, labelId));

let proposeContribution =
    (send, ~amountInteger, ~amountFraction, ~currency, ~description) =>
  send(
    ProposeContribution(amountInteger, amountFraction, currency, description)
  );

let component = ReasonReact.reducerComponent("SelectedVenture");

let make = (~venture as initialVenture, ~session: Session.Data.t, _children) => {
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
                     | Error(venture, _item, result) => {
                         Js.log("An error occured while synchronizing");
                         Js.log(result);
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
    | ProposePartner =>
      switch (String.trim(state.prospectId)) {
      | "" => ReasonReact.NoUpdate
      | prospectId =>
        ReasonReact.SideEffects(
          (
            ({send}) =>
              Js.Promise.(
                Cmd.ProposePartner.(
                  state.venture
                  |> exec(
                       session,
                       ~prospectId=prospectId |> UserId.fromString
                     )
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
    | EndorsePartner(processId) =>
      ReasonReact.SideEffects(
        (
          ({send}) =>
            Js.Promise.(
              Cmd.EndorsePartner.(
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
    | ProposePartnerLabel(partnerId, labelId) =>
      ReasonReact.SideEffects(
        (
          ({send}) =>
            Js.Promise.(
              Cmd.ProposePartnerLabel.(
                state.venture
                |> exec(session, ~partnerId, ~labelId)
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
    | EndorsePartnerLabel(processId) =>
      ReasonReact.SideEffects(
        (
          ({send}) =>
            Js.Promise.(
              Cmd.EndorsePartnerLabel.(
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
    | ProposeContribution(amountInteger, amountFraction, currency, description) =>
      ReasonReact.SideEffects(
        (
          ({send}) =>
            Js.Promise.(
              Cmd.ProposeContribution.(
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
    | EndorseContribution(processId) =>
      ReasonReact.SideEffects(
        (
          ({send}) =>
            Js.Promise.(
              Cmd.EndorseContribution.(
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
               <li key=(m.userId |> UserId.toString)>
                 <div>
                   (text(m.userId |> UserId.toString))
                   (text(" Labels: "))
                   (
                     text(
                       List.fold_left(
                         (state, label) =>
                           state ++ LabelId.toString(label) ++ ", ",
                         "",
                         m.labels
                       )
                     )
                   )
                 </div>
                 <div>
                   (text("Pending Labels: "))
                   <ul>
                     {
                       let pending =
                         Array.of_list(
                           ViewModel.getPendingPartnerLabels(
                             m.userId,
                             state.viewModel
                           )
                           |> List.map((partnerLabel: ViewModel.partnerLabel) =>
                                <li
                                  key=(
                                    partnerLabel.processId
                                    |> ProcessId.toString
                                  )>
                                  (
                                    text(
                                      "'"
                                      ++ LabelId.toString(
                                           partnerLabel.labelId
                                         )
                                      ++ "' endorsed by: "
                                      ++ List.fold_left(
                                           (state, partnerId) =>
                                             state ++ partnerId ++ " ",
                                           "",
                                           partnerLabel.supporters
                                           |> List.map(UserId.toString)
                                         )
                                    )
                                  )
                                  (
                                    if (partnerLabel.supporters
                                        |> List.mem(session.userId) == false) {
                                      <button
                                        onClick=(
                                          _e =>
                                            send(
                                              EndorsePartnerLabel(
                                                partnerLabel.processId
                                              )
                                            )
                                        )>
                                        (text("Endorse Partner Label"))
                                      </button>;
                                    } else {
                                      ReasonReact.nullElement;
                                    }
                                  )
                                </li>
                              )
                         );
                       ReasonReact.arrayToElement(pending);
                     }
                   </ul>
                 </div>
                 <PartnerLabelInput
                   submit=(proposePartnerLabel(send, m.userId))
                 />
               </li>
             )
        )
      );
    let prospects =
      ReasonReact.arrayToElement(
        Array.of_list(
          ViewModel.getProspects(state.viewModel)
          |> List.map((prospect: ViewModel.prospect) =>
               <li key=(prospect.userId |> UserId.toString)>
                 (
                   text(
                     "'"
                     ++ (prospect.userId |> UserId.toString)
                     ++ "' endorsed by: "
                     ++ List.fold_left(
                          (state, partnerId) => state ++ partnerId ++ " ",
                          "",
                          prospect.endorsedBy |> List.map(UserId.toString)
                        )
                   )
                 )
                 (
                   if (prospect.endorsedBy |> List.mem(session.userId) == false) {
                     <button
                       onClick=(_e => send(EndorsePartner(prospect.processId)))>
                       (text("Endorse Partner"))
                     </button>;
                   } else {
                     ReasonReact.nullElement;
                   }
                 )
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
               <li key=(contribution.processId |> ProcessId.toString)>
                 (
                   text(
                     "'"
                     ++ contribution.description
                     ++ "' endorsed by: "
                     ++ List.fold_left(
                          (state, partnerId) => state ++ partnerId ++ " ",
                          "",
                          contribution.supporters |> List.map(UserId.toString)
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
               <li key=(contribution.processId |> ProcessId.toString)>
                 (
                   text(
                     "'"
                     ++ contribution.description
                     ++ "' endorsed by: "
                     ++ List.fold_left(
                          (state, partnerId) => state ++ partnerId ++ " ",
                          "",
                          contribution.supporters |> List.map(UserId.toString)
                        )
                   )
                 )
                 (
                   if (contribution.supporters
                       |> List.mem(session.userId) == false) {
                     <button
                       onClick=(
                         _e =>
                           send(EndorseContribution(contribution.processId))
                       )>
                       (text("Endorse Contribution"))
                     </button>;
                   } else {
                     ReasonReact.nullElement;
                   }
                 )
               </li>
             )
        )
      );
    <div>
      <h2>
        (
          text(
            ViewModel.ventureName(state.viewModel)
            ++ " ("
            ++ Venture.getId(initialVenture)
            ++ ")"
          )
        )
      </h2>
      <h3> (text("Policies:")) </h3>
      <div>
        (
          text(
            "MetaPolicy - ActivationThreshold "
            ++ string_of_float(state.viewModel.metaPolicy.thresholdPercent)
            ++ "%"
          )
        )
      </div>
      <div>
        (
          text(
            "PartnerPolicy - ActivationThreshold "
            ++ string_of_float(state.viewModel.partnerPolicy.thresholdPercent)
            ++ "%"
          )
        )
      </div>
      <div>
        (
          text(
            "ContributionPolicy - ActivationThreshold "
            ++ string_of_float(
                 state.viewModel.contributionPolicy.thresholdPercent
               )
            ++ "%"
          )
        )
      </div>
      <div>
        (
          text(
            "PartnerLabelPolicy - ActivationThreshold "
            ++ string_of_float(
                 state.viewModel.partnerLabelPolicy.thresholdPercent
               )
            ++ "%"
          )
        )
      </div>
      <h3> (text("Contributions:")) </h3>
      <ul> contributions </ul>
      <h4> (text("Pending acceptance:")) </h4>
      <ul> contributionProcesses </ul>
      <ContributionInput submit=(proposeContribution(send)) />
      <h3> (text("Partners:")) </h3>
      <ul> partners </ul>
      <h4> (text("Prospects:")) </h4>
      <ul> prospects </ul>
      <input
        placeholder="BlockstackId"
        value=state.prospectId
        onChange=(e => send(changeNewPartnerId(e)))
        autoFocus=Js.false_
      />
      <button onClick=(_e => send(ProposePartner))>
        (text("Propose Partner"))
      </button>
    </div>;
  }
};
