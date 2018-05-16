include ViewCommon;

open PrimitiveTypes;

type state = {
  viewModel: ViewModel.t,
  selfRemoved: bool,
  balance: ViewModel.balance,
};

type action =
  | EndorsePartner(ProcessId.t)
  | EndorsePartnerRemoval(ProcessId.t)
  | ProposePayout(list((string, BTC.t)))
  | RejectPayout(ProcessId.t)
  | EndorsePayout(ProcessId.t);

let component = ReasonReact.reducerComponent("SelectedVenture");

module Styles = {
  open Css;
  let flexSpaceBetween =
    style([display(`flex), justifyContent(`spaceBetween)]);
};

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
  willReceiveProps: (_) => {
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
    | (false, ProposePayout(destinations)) =>
      commands.proposePayout(
        ~accountIdx=WalletTypes.AccountIndex.default,
        ~destinations,
        ~fee=BTC.fromSatoshis(100L),
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
          |> List.map((partner: ViewModel.partner) =>
               <Partner key=(partner.userId |> UserId.toString) partner />
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
    let transactions =
      ReasonReact.array(
        Array.of_list(
          {
            let (confirmed, unconfirmed) =
              state.viewModel |> ViewModel.transactions;
            List.append(
              unconfirmed
              |> List.mapi((iter, tx: ViewModel.unconfirmedTx) =>
                   <Transaction
                     tx=(Unconfirmed(tx))
                     key=(iter |> string_of_int)
                   />
                 ),
              confirmed
              |> List.mapi((iter, tx: ViewModel.confirmedTx) =>
                   <Transaction
                     tx=(Confirmed(tx))
                     key=(string_of_int(iter + List.length(unconfirmed)))
                   />
                 ),
            )
            |> Utils.intersperse(key => <MDivider key />);
          },
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
    <Body4
      titles=["Partners", "Transactions"]
      body1=
        <div>
          <MTypography variant=`Title>
            (ViewModel.ventureName(state.viewModel) |> text)
          </MTypography>
          <MTypography variant=`Display2>
            <b key="currentSpendable">
              (state.balance.currentSpendable |> BTC.format |> text)
            </b>
            ("BTC" |> text)
          </MTypography>
          <MTypography variant=`Subheading>
            <b key="reserved">
              (BTC.format(state.balance.reserved) |> text)
            </b>
            (" BTC IN RESERVE" |> text)
          </MTypography>
        </div>
      body2=
        <div className=Styles.flexSpaceBetween>
          <MFabButton
            variant=Aqua
            route=(Venture(ViewModel.ventureId(state.viewModel), Receive))>
            ("RECEIVE" |> text)
          </MFabButton>
          <MFabButton
            variant=Orange
            route=(Venture(ViewModel.ventureId(state.viewModel), Payout))>
            ("PAY OUT" |> text)
          </MFabButton>
        </div>
      body3=
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
            ("Add or Remove Partners" |> text)
          </LinkButton>
        </div>
      body4=
        <div>
          <h3> (text("Wallet:")) </h3>
          <PayoutInput
            onSend=(destinations => send(ProposePayout(destinations)))
          />
          <h4> (text("Payout processes:")) </h4>
          <ul> payouts </ul>
          <MaterialUi.List> transactions </MaterialUi.List>
        </div>
    />;
  },
};
