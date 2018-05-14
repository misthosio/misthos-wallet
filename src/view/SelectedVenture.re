open PrimitiveTypes;

open WalletTypes;

let text = Utils.text;

type state = {
  viewModel: ViewModel.t,
  selfRemoved: bool,
  balance: ViewModel.Wallet.balance,
};

type action =
  | EndorsePartner(ProcessId.t)
  | EndorsePartnerRemoval(ProcessId.t)
  | GetIncomeAddress
  | ProposePayout(list((string, BTC.t)))
  | RejectPayout(ProcessId.t)
  | EndorsePayout(ProcessId.t);

let component = ReasonReact.reducerComponent("SelectedVenture");

let make =
    (
      ~venture as initialViewModel,
      ~session: Session.Data.t,
      ~commands: VentureWorkerClient.Cmd.t,
      _children,
    ) => {
  ...component,
  initialState: () => {
    viewModel: initialViewModel,
    selfRemoved:
      initialViewModel |> ViewModel.isPartner(session.userId) == false,
    balance: initialViewModel |> ViewModel.balance,
  },
  willReceiveProps: ({state}) => {
    ...state,
    viewModel: initialViewModel,
    selfRemoved:
      initialViewModel |> ViewModel.isPartner(session.userId) == false,
    balance: initialViewModel |> ViewModel.balance,
  },
  reducer: (action, state) =>
    switch (state.selfRemoved, action) {
    | (false, EndorsePartner(processId)) =>
      commands.endorsePartner(~processId);
      ReasonReact.NoUpdate;
    | (false, EndorsePartnerRemoval(processId)) =>
      commands.endorsePartnerRemoval(~processId);
      ReasonReact.NoUpdate;
    | (false, GetIncomeAddress) =>
      commands.exposeIncomeAddress(~accountIdx=AccountIndex.default);
      ReasonReact.NoUpdate;
    | (false, ProposePayout(destinations)) =>
      commands.proposePayout(
        ~accountIdx=WalletTypes.AccountIndex.default,
        ~destinations,
        ~fee=BTC.fromSatoshis(5L),
      );
      ReasonReact.NoUpdate;
    | (false, RejectPayout(processId)) =>
      commands.rejectPayout(~processId);
      ReasonReact.NoUpdate;
    | (false, EndorsePayout(processId)) =>
      commands.endorsePayout(~processId);
      ReasonReact.NoUpdate;
    | _ => ReasonReact.NoUpdate
    },
  render: ({send, state}) => {
    let partners =
      ReasonReact.array(
        Array.of_list(
          ViewModel.partners(state.viewModel)
          |> List.map((partner: ViewModel.partner) => <Partner partner />),
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
                        )
                     ++ " rejected by: "
                     ++ List.fold_left(
                          (state, partnerId) => state ++ partnerId ++ " ",
                          "",
                          payout.rejectedBy |> List.map(UserId.toString),
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
                 (
                   switch (
                     payout.status,
                     payout.rejectedBy |> List.mem(session.userId),
                     payout.endorsedBy |> List.mem(session.userId),
                   ) {
                   | (PayoutPending, false, false) =>
                     <button
                       onClick=(_e => send(RejectPayout(payout.processId)))>
                       (text("Reject Payout"))
                     </button>
                   | _ => ReasonReact.null
                   }
                 )
               </li>
             ),
        ),
      );
    <Body3
      titles=["Partners", "Transactions"]
      body1=
        <div>
          <MTypography variant=`Title>
            (ViewModel.ventureName(state.viewModel) |> Utils.text)
          </MTypography>
          <MTypography variant=`Display2>
            <b>
              (state.balance.currentSpendable |> BTC.format |> Utils.text)
            </b>
            ("BTC" |> Utils.text)
          </MTypography>
          <MTypography variant=`Subheading>
            <b> (BTC.format(state.balance.reserved) |> Utils.text) </b>
            (" BTC IN RESERVE" |> Utils.text)
          </MTypography>
        </div>
      body2=
        <div>
          (
            switch (state.selfRemoved) {
            | true =>
              <b>
                (
                  text(
                    "YOU HAVE BEEN REMOVED FROM THIS VENTURE; VENTURE IS IN READ ONLY",
                  )
                )
              </b>
            | _ => ReasonReact.null
            }
          )
          <MaterialUi.List disablePadding=true> partners </MaterialUi.List>
          <h4> (text("Prospects:")) </h4>
          <ul> prospects </ul>
          <h4> (text("To be removed:")) </h4>
          <ul> removalProspects </ul>
          <LinkButton
            fullWidth=true
            route=(
              Venture(ViewModel.ventureId(state.viewModel), ManagePartners)
            )>
            ("Add or Remove Partners" |> Utils.text)
          </LinkButton>
        </div>
      body3=
        <div>
          <h3> (text("Wallet:")) </h3>
          <h4> (text("Income Addresses:")) </h4>
          <ul> addresses </ul>
          <button onClick=(_e => send(GetIncomeAddress))>
            (text("Get New Income Address"))
          </button>
          <Payout
            onSend=(destinations => send(ProposePayout(destinations)))
          />
          <h4> (text("Payouts:")) </h4>
          <ul> payouts </ul>
        </div>
    />;
  },
};
