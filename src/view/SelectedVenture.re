include ViewCommon;

module ViewData = ViewModel.SelectedVentureView;

open PrimitiveTypes;

type state = {viewData: ViewData.t};

type action =
  | EndorsePartner(ProcessId.t)
  | EndorsePartnerRemoval(ProcessId.t)
  | RejectPayout(ProcessId.t)
  | EndorsePayout(ProcessId.t);

let component = ReasonReact.reducerComponent("SelectedVenture");

let make =
    (
      ~viewData: ViewData.t,
      ~session: Session.Data.t,
      ~commands: VentureWorkerClient.Cmd.t,
      _children,
    ) => {
  ...component,
  initialState: () => {viewData: viewData},
  willReceiveProps: (_) => {viewData: viewData},
  reducer: (action, state) =>
    switch (state.viewData.readOnly, action) {
    | (false, EndorsePartner(processId)) =>
      commands.endorsePartner(~processId);
      ReasonReact.NoUpdate;
    | (false, EndorsePartnerRemoval(processId)) =>
      commands.endorsePartnerRemoval(~processId);
      ReasonReact.NoUpdate;
    | (false, RejectPayout(processId)) =>
      commands.rejectPayout(~processId);
      ReasonReact.NoUpdate;
    | (false, EndorsePayout(processId)) =>
      commands.endorsePayout(~processId);
      ReasonReact.NoUpdate;
    | _ => ReasonReact.NoUpdate
    },
  render: ({send, state: {viewData}}) => {
    let partners =
      ReasonReact.array(
        Array.of_list(
          viewData.partners
          |> List.map((partner: ViewData.partner) =>
               <Partner
                 key=(partner.userId |> UserId.toString)
                 partnerId=partner.userId
                 name=?partner.name
               />
             ),
        ),
      );
    let prospects =
      ReasonReact.array(
        Array.of_list(
          viewData.prospects
          |> List.map((prospect: ViewData.prospect) =>
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
          viewData.removalProspects
          |> List.map((prospect: ViewData.prospect) =>
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
    let payouts =
      viewData.payoutsPendingApproval
      |> List.map((payout: ViewData.payout) =>
           MaterialUi.(
             <ListItem
               button=true
               onClick=(
                 Router.clickToRoute(
                   Venture(viewData.ventureId, Payout(payout.processId)),
                 )
               )
               key=(payout.processId |> ProcessId.toString)>
               (
                 text(
                   "'"
                   ++ (payout.processId |> ProcessId.toString)
                   ++ "' - "
                   ++ BTC.format(payout.summary.spentWithFees),
                 )
               )
             </ListItem>
           )
         );
    let transactions =
      ReasonReact.array(
        Array.of_list(
          {
            let unconfirmed = viewData.unconfirmedTxs;
            let confirmed = viewData.confirmedTxs;
            Belt.List.concatMany([|
              payouts,
              unconfirmed
              |> List.mapi((iter, tx: ViewData.txData) =>
                   <Transaction tx key=(iter |> string_of_int) />
                 ),
              confirmed
              |> List.mapi((iter, tx: ViewData.txData) =>
                   <Transaction
                     tx
                     key=(string_of_int(iter + List.length(unconfirmed)))
                   />
                 ),
            |])
            |> Utils.intersperse(key => <MDivider key />);
          },
        ),
      );
    <Body4
      titles=["Partners", "Transactions"]
      body1=
        <div>
          <MTypography variant=`Title>
            (viewData.ventureName |> text)
          </MTypography>
          <Balance
            currentSpendable=viewData.balance.currentSpendable
            reserved=viewData.balance.reserved
          />
        </div>
      body2=
        <div className=Css.(style([display(`flex)]))>
          <MFabButton
            variant=Aqua route=(Venture(viewData.ventureId, Receive))>
            ("RECEIVE" |> text)
          </MFabButton>
          <div className=Css.(style([width(px(Theme.space(8)))])) />
          <MFabButton
            variant=Orange route=(Venture(viewData.ventureId, CreatePayout))>
            ("PAY OUT" |> text)
          </MFabButton>
        </div>
      body3=
        <div>
          (
            switch (viewData.readOnly) {
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
            route=(Venture(viewData.ventureId, ManagePartners))>
            ("Add or Remove Partners" |> text)
          </LinkButton>
        </div>
      body4=<div> <MaterialUi.List> transactions </MaterialUi.List> </div>
    />;
  },
};
