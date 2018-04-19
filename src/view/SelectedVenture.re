open Venture;

open PrimitiveTypes;

let text = ReasonReact.stringToElement;

type state = {
  venture: Venture.t(ViewModel.t),
  viewModel: ViewModel.t,
  prospectId: string,
  balance: Venture.Wallet.balance,
};

type action =
  | ChangeNewPartnerId(string)
  | ProposePartner
  | EndorsePartner(ProcessId.t)
  | RemovePartner(UserId.t)
  | EndorsePartnerRemoval(ProcessId.t)
  | GetIncomeAddress
  | ProposePayout(list((string, BTC.t)))
  | EndorsePayout(ProcessId.t);

let changeNewPartnerId = event =>
  ChangeNewPartnerId(
    ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value,
  );

let component = ReasonReact.reducerComponent("SelectedVenture");

let make =
    (
      ~venture as initialVenture,
      ~updateVenture,
      ~session: Session.Data.t,
      _children,
    ) => {
  ...component,
  initialState: () => {
    balance: initialVenture |> Venture.Wallet.balance,
    venture: initialVenture,
    viewModel: Venture.getListenerState(initialVenture),
    prospectId: "",
  },
  willReceiveProps: ({state}) => {
    ...state,
    balance: initialVenture |> Venture.Wallet.balance,
    viewModel: initialVenture |> Venture.getListenerState,
    venture: initialVenture,
  },
  reducer: (action, state) =>
    switch (action) {
    | ChangeNewPartnerId(text) =>
      ReasonReact.Update({...state, prospectId: text})
    | ProposePartner =>
      switch (String.trim(state.prospectId)) {
      | "" => ReasonReact.NoUpdate
      | prospectId =>
        ReasonReact.SideEffects(
          (
            (_) =>
              Js.Promise.(
                Cmd.ProposePartner.(
                  state.venture
                  |> exec(~prospectId=prospectId |> UserId.fromString)
                  |> then_(result =>
                       (
                         switch (result) {
                         | Ok(venture) => updateVenture(venture)
                         | PartnerAlreadyExists =>
                           Js.log("PartnerAlreadyExists")
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
          (_) =>
            Js.Promise.(
              Cmd.EndorsePartner.(
                state.venture
                |> exec(~processId)
                |> then_(result =>
                     (
                       switch (result) {
                       | Ok(venture) => updateVenture(venture)
                       }
                     )
                     |> resolve
                   )
                |> ignore
              )
            )
        ),
      )
    | RemovePartner(partnerId) =>
      ReasonReact.SideEffects(
        (
          (_) =>
            Js.Promise.(
              Cmd.ProposePartnerRemoval.(
                state.venture
                |> exec(~partnerId)
                |> then_(result =>
                     (
                       switch (result) {
                       | Ok(venture) => updateVenture(venture)
                       | PartnerDoesNotExist => Js.log("PartnerDoesNotExist")
                       }
                     )
                     |> resolve
                   )
                |> ignore
              )
            )
        ),
      )
    | EndorsePartnerRemoval(processId) =>
      ReasonReact.SideEffects(
        (
          (_) =>
            Js.Promise.(
              Cmd.EndorsePartnerRemoval.(
                state.venture
                |> exec(~processId)
                |> then_(result =>
                     (
                       switch (result) {
                       | Ok(venture) => updateVenture(venture)
                       }
                     )
                     |> resolve
                   )
                |> ignore
              )
            )
        ),
      )
    | GetIncomeAddress =>
      ReasonReact.SideEffects(
        (
          (_) =>
            Js.Promise.(
              Cmd.ExposeIncomeAddress.(
                state.venture
                |> exec(~accountIdx=WalletTypes.AccountIndex.default)
                |> then_(result =>
                     (
                       switch (result) {
                       | Ok(_, venture) => updateVenture(venture)
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
          (_) =>
            Js.Promise.(
              Cmd.ProposePayout.(
                state.venture
                |> exec(
                     ~accountIdx=WalletTypes.AccountIndex.default,
                     ~destinations,
                     ~fee=BTC.fromSatoshis(5L),
                   )
                |> then_(result =>
                     (
                       switch (result) {
                       | Ok(venture) => updateVenture(venture)
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
          (_) =>
            Js.Promise.(
              Cmd.EndorsePayout.(
                state.venture
                |> exec(~processId)
                |> then_(result =>
                     (
                       switch (result) {
                       | Ok(venture) => updateVenture(venture)
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
                 <div>
                   (text(m.userId |> UserId.toString))
                   (
                     switch (
                       m.userId |> UserId.eq(session.userId),
                       ViewModel.removalProspects(state.viewModel)
                       |> List.exists((p: ViewModel.prospect) =>
                            UserId.eq(p.userId, m.userId)
                          ),
                     ) {
                     | (false, false) =>
                       <button onClick=(_e => send(RemovePartner(m.userId)))>
                         (text("Propose Removal"))
                       </button>
                     | _ => ReasonReact.nullElement
                     }
                   )
                 </div>
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
    let removalProspects =
      ReasonReact.arrayToElement(
        Array.of_list(
          ViewModel.removalProspects(state.viewModel)
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
                         _e =>
                           send(EndorsePartnerRemoval(prospect.processId))
                       )>
                       (text("Endorse Removal"))
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
          |> ViewModel.payouts
          |> List.map((payout: ViewModel.payout) =>
               <li key=(payout.processId |> ProcessId.toString)>
                 (
                   text(
                     "'"
                     ++ (payout.processId |> ProcessId.toString)
                     ++ "' status: "
                     ++ (
                       switch (payout.status) {
                       | PayoutPending => "pending"
                       | PayoutCompleted(id) =>
                         "completed (txId: " ++ id ++ ")"
                       | PayoutFailed(reason) =>
                         "failed (error: '" ++ reason ++ "')"
                       }
                     )
                     ++ " endorsed by: "
                     ++ List.fold_left(
                          (state, partnerId) => state ++ partnerId ++ " ",
                          "",
                          payout.endorsedBy |> List.map(UserId.toString),
                        ),
                   )
                 )
                 (
                   switch (
                     payout.status,
                     payout.endorsedBy |> List.mem(session.userId),
                   ) {
                   | (PayoutPending, false) =>
                     <button
                       onClick=(_e => send(EndorsePayout(payout.processId)))>
                       (text("Endorse Payout"))
                     </button>
                   | _ => ReasonReact.nullElement
                   }
                 )
               </li>
             ),
        ),
      );
    <div>
      <div>
        <h2> (text(ViewModel.ventureName(state.viewModel))) </h2>
        (
          text(
            "Join Venture url: "
            ++ Location.origin
            ++ Router.Config.routeToUrl(
                 JoinVenture(initialVenture |> Venture.getId, session.userId),
               ),
          )
        )
        <h3> (text("Partners:")) </h3>
        <ul> partners </ul>
        <h4> (text("Prospects:")) </h4>
        <ul> prospects </ul>
        <h4> (text("To be removed:")) </h4>
        <ul> removalProspects </ul>
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
          text(
            "income: "
            ++ BTC.format(state.balance.income)
            ++ " spent: "
            ++ BTC.format(state.balance.spent)
            ++ " reserved: "
            ++ BTC.format(state.balance.reserved),
          )
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
