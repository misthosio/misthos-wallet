open Venture;

open PrimitiveTypes;

let text = ReasonReact.stringToElement;

type state = {
  venture: Venture.t,
  viewModel: ViewModel.t,
  prospectId: string,
  balance: option(Venture.Wallet.balance),
  worker: ref(Worker.t),
};

type action =
  | WorkerMessage(Worker.Message.receive)
  | ChangeNewPartnerId(string)
  | UpdateVenture(Venture.t)
  | ProposePartner
  | UpdateBalance(Venture.Wallet.balance)
  | EndorsePartner(ProcessId.t)
  | GetIncomeAddress
  | ProposePayout(list((string, BTC.t)))
  | EndorsePayout(ProcessId.t);

let changeNewPartnerId = event =>
  ChangeNewPartnerId(
    ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value,
  );

let component = ReasonReact.reducerComponent("SelectedVenture");

let make = (~venture as initialVenture, ~session: Session.Data.t, _children) => {
  ...component,
  initialState: () => {
    balance: None,
    venture: initialVenture,
    viewModel: Venture.getViewModel(initialVenture),
    prospectId: "",
    worker: ref(Worker.make(~onMessage=Js.log)),
  },
  subscriptions: ({send, state}) => [
    Sub(
      () => {
        Worker.terminate(state.worker^);
        let worker =
          Worker.make(~onMessage=message => send(WorkerMessage(message)));
        Js.Promise.(
          Venture.getPartnerHistoryUrls(initialVenture)
          |> then_(urls =>
               Worker.Message.RegularlyFetch(
                 urls,
                 Venture.getSummary(initialVenture),
               )
               |> Worker.postMessage(worker)
               |> resolve
             )
          |> ignore
        );
        state.worker := worker;
        worker;
      },
      Worker.terminate,
    ),
  ],
  didMount: _self =>
    ReasonReact.SideEffects(
      ({send}) =>
        Js.Promise.(
          initialVenture
          |> Venture.Wallet.balance
          |> then_(balance => send(UpdateBalance(balance)) |> resolve)
          |> catch(error => Utils.printError("whoops", error) |> resolve)
          |> ignore
        ),
    ),
  reducer: (action, state) =>
    switch (action) {
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
                     | Ok(venture) =>
                       send(UpdateVenture(venture)) |> resolve
                     | Error(venture, _item, result) => {
                         Js.log("An error occured while synchronizing");
                         Js.log(result);
                         send(UpdateVenture(venture)) |> resolve;
                       },
                   )
                |> ignore
              )
            )
        ),
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
                  |> exec(~prospectId=prospectId |> UserId.fromString)
                  |> then_(result =>
                       (
                         switch (result) {
                         | Ok(venture) => send(UpdateVenture(venture))
                         | NoUserInfo => Js.log("NoUserInfo")
                         }
                       )
                       |> resolve
                     )
                  |> ignore
                )
              )
          ),
        )
      }
    | EndorsePartner(processId) =>
      ReasonReact.SideEffects(
        (
          ({send}) =>
            Js.Promise.(
              Cmd.EndorsePartner.(
                state.venture
                |> exec(~processId)
                |> then_(result =>
                     (
                       switch (result) {
                       | Ok(venture) => send(UpdateVenture(venture))
                       }
                     )
                     |> resolve
                   )
                |> ignore
              )
            )
        ),
      )
    | UpdateVenture(venture) =>
      Js.Promise.(
        Venture.getPartnerHistoryUrls(venture)
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
        viewModel: Venture.getViewModel(venture),
      });
    | UpdateBalance(balance) =>
      ReasonReact.Update({...state, balance: Some(balance)})
    | GetIncomeAddress =>
      ReasonReact.SideEffects(
        (
          ({send}) =>
            Js.Promise.(
              Cmd.ExposeIncomeAddress.(
                state.venture
                |> exec(~accountIdx=WalletTypes.AccountIndex.default)
                |> then_(result =>
                     (
                       switch (result) {
                       | Ok(_, venture) => send(UpdateVenture(venture))
                       }
                     )
                     |> resolve
                   )
                |> ignore
              )
            )
        ),
      )
    | ProposePayout(destinations) =>
      ReasonReact.SideEffects(
        (
          ({send}) =>
            Js.Promise.(
              Cmd.ProposePayout.(
                state.venture
                |> exec(
                     ~accountIdx=WalletTypes.AccountIndex.default,
                     ~destinations,
                     ~fee=BTC.fromSatoshis(1L),
                   )
                |> then_(result =>
                     (
                       switch (result) {
                       | Ok(venture) => send(UpdateVenture(venture))
                       }
                     )
                     |> resolve
                   )
                |> ignore
              )
            )
        ),
      )
    | EndorsePayout(processId) =>
      ReasonReact.SideEffects(
        (
          ({send}) =>
            Js.Promise.(
              Cmd.EndorsePayout.(
                state.venture
                |> exec(~processId)
                |> then_(result =>
                     (
                       switch (result) {
                       | Ok(venture) => send(UpdateVenture(venture))
                       }
                     )
                     |> resolve
                   )
                |> ignore
              )
            )
        ),
      )
    },
  render: ({send, state}) => {
    let partners =
      ReasonReact.arrayToElement(
        Array.of_list(
          ViewModel.partners(state.viewModel)
          |> List.map((m: ViewModel.partner) =>
               <li key=(m.userId |> UserId.toString)>
                 <div> (text(m.userId |> UserId.toString)) </div>
               </li>
             ),
        ),
      );
    let prospects =
      ReasonReact.arrayToElement(
        Array.of_list(
          ViewModel.prospects(state.viewModel)
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
                          prospect.endorsedBy |> List.map(UserId.toString),
                        ),
                   )
                 )
                 (
                   if (prospect.endorsedBy |> List.mem(session.userId) == false) {
                     <button
                       onClick=(
                         _e => send(EndorsePartner(prospect.processId))
                       )>
                       (text("Endorse Partner"))
                     </button>;
                   } else {
                     ReasonReact.nullElement;
                   }
                 )
               </li>
             ),
        ),
      );
    let addresses =
      ReasonReact.arrayToElement(
        Array.of_list(
          ViewModel.incomeAddresses(state.viewModel)
          |> List.map(address => <li key=address> (text(address)) </li>),
        ),
      );
    let payouts =
      ReasonReact.arrayToElement(
        Array.of_list(
          state.viewModel
          |> ViewModel.pendingPayouts
          |> List.map((payout: ViewModel.payout) =>
               <li key=(payout.processId |> ProcessId.toString)>
                 (
                   text(
                     "'"
                     ++ (payout.processId |> ProcessId.toString)
                     ++ "' endorsed by: "
                     ++ List.fold_left(
                          (state, partnerId) => state ++ partnerId ++ " ",
                          "",
                          payout.endorsedBy |> List.map(UserId.toString),
                        ),
                   )
                 )
                 (
                   if (payout.endorsedBy |> List.mem(session.userId) == false) {
                     <button
                       onClick=(_e => send(EndorsePayout(payout.processId)))>
                       (text("Endorse Payout"))
                     </button>;
                   } else {
                     ReasonReact.nullElement;
                   }
                 )
               </li>
             ),
        ),
      );
    <div>
      <div>
        <h2>
          (
            text(
              ViewModel.ventureName(state.viewModel)
              ++ " ("
              ++ Venture.getId(initialVenture)
              ++ ")",
            )
          )
        </h2>
        <h3> (text("Policies:")) </h3>
        <div>
          (
            text(
              "MetaPolicy - ActivationThreshold "
              ++ string_of_float(state.viewModel.metaPolicy.thresholdPercent)
              ++ "%",
            )
          )
        </div>
        <div>
          (
            text(
              "PartnerPolicy - ActivationThreshold "
              ++ string_of_float(
                   state.viewModel.partnerPolicy.thresholdPercent,
                 )
              ++ "%",
            )
          )
        </div>
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
        <h3> (text("Wallet:")) </h3>
        <h4> (text("blance: ")) </h4>
        (
          switch (state.balance) {
          | None => text("loading")
          | Some(balance) =>
            text(
              "total: "
              ++ BTC.format(balance.total)
              ++ " reserved: "
              ++ BTC.format(balance.reserved),
            )
          }
        )
        <h4> (text("Income Addresses:")) </h4>
        <ul> addresses </ul>
        <button onClick=(_e => send(GetIncomeAddress))>
          (text("Get New Income Address"))
        </button>
        <Payout onSend=(destinations => send(ProposePayout(destinations))) />
        <h4> (text("Payouts:")) </h4>
        <ul> payouts </ul>
      </div>
    </div>;
  },
};
