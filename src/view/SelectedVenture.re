open PrimitiveTypes;

let text = Utils.text;

type state = {
  viewModel: ViewModel.t,
  prospectId: string,
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
      ~venture as initialViewModel,
      ~session: Session.Data.t,
      ~commands: VentureWorkerClient.Cmd.t,
      _children,
    ) => {
  ...component,
  initialState: () => {viewModel: initialViewModel, prospectId: ""},
  willReceiveProps: ({state}) => {...state, viewModel: initialViewModel},
  reducer: (action, state) =>
    switch (action) {
    | ChangeNewPartnerId(text) =>
      ReasonReact.Update({...state, prospectId: text})
    | ProposePartner =>
      switch (String.trim(state.prospectId)) {
      | "" => ReasonReact.NoUpdate
      | prospectId =>
        commands.proposePartner(~prospectId=prospectId |> UserId.fromString);
        ReasonReact.Update({...state, prospectId: ""});
      }
    | EndorsePartner(processId) =>
      ReasonReact.SideEffects(((_) => commands.endorsePartner(~processId)))
    | RemovePartner(partnerId) =>
      ReasonReact.SideEffects(
        ((_) => commands.proposePartnerRemoval(~partnerId)),
      )
    | EndorsePartnerRemoval(processId) =>
      ReasonReact.SideEffects(
        ((_) => commands.endorsePartnerRemoval(~processId)),
      )
    | GetIncomeAddress =>
      ReasonReact.SideEffects(
        (
          (_) =>
            ()
            /* Js.Promise.( */
            /*   Cmd.ExposeIncomeAddress.( */
            /*     state.venture */
            /*     |> exec(~accountIdx=WalletTypes.AccountIndex.default) */
            /*     |> then_(result => */
            /*          ( */
            /*            switch (result) { */
            /*            | Ok(_, venture) => updateVenture(venture) */
            /*            } */
            /*          ) */
            /*          |> resolve */
            /*        ) */
            /*     |> ignore */
            /*   ) */
            /* ) */
        ),
      )
    | ProposePayout(destinations) =>
      ReasonReact.SideEffects(
        (
          (_) =>
            commands.proposePayout(
              ~accountIdx=WalletTypes.AccountIndex.default,
              ~destinations,
              ~fee=BTC.fromSatoshis(5L),
            )
        ),
      )
    | EndorsePayout(processId) =>
      ReasonReact.SideEffects(((_) => commands.endorsePayout(~processId)))
    },
  render: ({send, state}) => {
    let partners =
      ReasonReact.array(
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
                     | _ => ReasonReact.null
                     }
                   )
                 </div>
               </li>
             ),
        ),
      );
    let prospects =
      ReasonReact.array(
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
                     ReasonReact.null;
                   }
                 )
               </li>
             ),
        ),
      );
    let removalProspects =
      ReasonReact.array(
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
                     ReasonReact.null;
                   }
                 )
               </li>
             ),
        ),
      );
    let addresses =
      ReasonReact.array(
        Array.of_list(
          ViewModel.incomeAddresses(state.viewModel)
          |> List.map(address => <li key=address> (text(address)) </li>),
        ),
      );
    let payouts =
      ReasonReact.array(
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
                   | _ => ReasonReact.null
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
                 JoinVenture(initialViewModel.ventureId, session.userId),
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
          autoFocus=false
        />
        <button onClick=(_e => send(ProposePartner))>
          (text("Propose Partner"))
        </button>
        <h3> (text("Wallet:")) </h3>
        <h4> (text("blance: ")) </h4>
        (
          text(
            "income: ",
            /* ++ BTC.format(state.balance.income) */
            /* ++ " spent: " */
            /* ++ BTC.format(state.balance.spent) */
            /* ++ " reserved: " */
            /* ++ BTC.format(state.balance.reserved), */
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
