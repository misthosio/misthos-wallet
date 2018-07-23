open Belt;

include ViewCommon;

module ViewData = ViewModel.SelectedVentureView;

open PrimitiveTypes;

type state = {
  viewData: ViewData.t,
  loggedInStatus: UserId.map(bool),
};

type action =
  | SetHasLoggedIn(userId, bool);

let component = ReasonReact.reducerComponent("SelectedVenture");

module Styles = {
  open Css;
  let addressesButtonIcon =
    style([
      marginTop(px(Theme.space(2) * (-1))),
      marginBottom(px(Theme.space(1) * (-1))),
      transform(rotate(deg(90))),
    ]);
  let atRiskAddressButtonIcon =
    style([
      marginTop(px(Theme.space(2) * (-1))),
      marginBottom(px(Theme.space(1) * (-1))),
    ]);
};

let updateLoggedInStatus = (partners, send) =>
  partners
  |. List.keep((p: ViewData.partner) => p.joinedWallet == false)
  |. List.forEach((p: ViewData.partner) =>
       Js.Promise.(
         p.hasLoggedIn
         |> then_(known => send(SetHasLoggedIn(p.userId, known)) |> resolve)
       )
       |> ignore
     );

let make = (~viewData: ViewData.t, _children) => {
  ...component,
  initialState: () => {viewData, loggedInStatus: UserId.makeMap()},
  willReceiveProps: ({state, send}) => {
    updateLoggedInStatus(viewData.partners, send);
    {viewData, loggedInStatus: state.loggedInStatus};
  },
  reducer: (action, state) =>
    switch (action) {
    | SetHasLoggedIn(userId, known) =>
      ReasonReact.Update({
        ...state,
        loggedInStatus: state.loggedInStatus |. Map.set(userId, known),
      })
    },
  didMount: ({send}) => updateLoggedInStatus(viewData.partners, send),
  render: ({state: {viewData, loggedInStatus}}) => {
    let warning =
      switch (Environment.get().network) {
      | Testnet => Some(WarningsText.testnet)
      | _ => None
      };
    let getPartnerStatusChip =
        (~endorsed: bool, ~joinedWallet: bool, ~hasLoggedIn: option(bool)) =>
      switch (endorsed, joinedWallet, hasLoggedIn) {
      | (false, _, _) => <StatusChip status=Pending label="PENDING" />
      | (true, false, Some(false)) =>
        <StatusChip status=Pending label="SIGN IN REQUIRED" />
      | (true, false, _) =>
        <StatusChip status=Pending label="SYNC REQUIRED" />
      | (true, true, _) => ReasonReact.null
      };
    let alerts =
      viewData.prospects
      |. List.keepMap((prospect: ViewData.prospect) =>
           prospect.canVote ?
             Some(
               <AlertListItem
                 icon=(
                   switch (prospect.data.processType) {
                   | Removal => Minus
                   | Addition => Plus
                   }
                 )
                 onClick=(
                   Router.clickToRoute(
                     Venture(
                       viewData.ventureId,
                       Partner(prospect.processId),
                     ),
                   )
                 )
                 key=(prospect.processId |> ProcessId.toString)
                 primary=(
                   text(
                     (
                       switch (prospect.data.processType) {
                       | Removal => "Removal"
                       | Addition => "Addition"
                       }
                     )
                     ++ " of '"
                     ++ UserId.toString(prospect.data.userId)
                     ++ "'",
                   )
                 )
                 secondary=(
                   text(
                     "proposed by " ++ UserId.toString(prospect.proposedBy),
                   )
                 )
               />,
             ) :
             None
         );
    let prospects =
      viewData.prospects
      |. List.map((partner: ViewData.prospect) =>
           <Partner
             key=(UserId.toString(partner.data.userId) ++ "-prospect")
             partnerId=partner.data.userId
             onClick=(
               Router.clickToRoute(
                 Venture(viewData.ventureId, Partner(partner.processId)),
               )
             )
             status=(
               getPartnerStatusChip(
                 ~endorsed=false,
                 ~joinedWallet=false,
                 ~hasLoggedIn=Some(false),
               )
             )
           />
         );
    let currentPartners =
      viewData.partners
      |. List.map((partner: ViewData.partner) =>
           <Partner
             key=(partner.userId |> UserId.toString)
             partnerId=partner.userId
             name=?partner.name
             status=(
               getPartnerStatusChip(
                 ~endorsed=true,
                 ~joinedWallet=partner.joinedWallet,
                 ~hasLoggedIn=loggedInStatus |. Map.get(partner.userId),
               )
             )
           />
         );
    let stickyHeader = (~first=false, header) => [
      <MListSubheader first> (header |> text) </MListSubheader>,
    ];
    let partners = {
      let showHeaders = List.length(prospects) != 0;
      ReasonReact.array(
        List.toArray(
          List.concatMany([|
            alerts,
            showHeaders ? stickyHeader(~first=true, "Pending Approval") : [],
            prospects,
            showHeaders ? stickyHeader("Current") : [],
            currentPartners,
          |]),
        ),
      );
    };
    let payouts =
      ReasonReact.array(
        List.toArray(
          viewData.payoutsPendingApproval
          |. List.map(
               (
                 {proposedBy, processId, data: {summary}}: ViewData.payoutProcess,
               ) =>
               <AlertListItem
                 icon=ArrowUp
                 onClick=(
                   Router.clickToRoute(
                     Venture(viewData.ventureId, Payout(processId)),
                   )
                 )
                 key=(processId |> ProcessId.toString)
                 primary=(
                   text(
                     "Payout of "
                     ++ BTC.format(summary.spentWithFees)
                     ++ " BTC",
                   )
                 )
                 secondary=(
                   text("proposed by " ++ UserId.toString(proposedBy))
                 )
               />
             ),
        ),
      );
    let transactions =
      ReasonReact.array(
        List.toArray(
          {
            let unconfirmed = viewData.unconfirmedTxs;
            let confirmed = viewData.confirmedTxs;
            List.concatMany([|
              unconfirmed
              |. List.mapWithIndex((iter, tx: ViewData.txData) => {
                   let (txType, primary) =
                     switch (tx.txType) {
                     | Payout => (Transaction.Payout, "unconfirmed payout")
                     | Income => (Transaction.Income, "unconfirmed income")
                     };
                   <Transaction
                     txType
                     primary
                     amount=tx.amount
                     date=tx.date
                     onClick=(Router.clickToRoute(tx.detailsLink))
                     key=(iter |> string_of_int)
                   />;
                 }),
              confirmed
              |. List.mapWithIndex((iter, tx: ViewData.txData) => {
                   let (txType, primary) =
                     switch (tx.txType) {
                     | Payout => (Transaction.Payout, "payout")
                     | Income => (Transaction.Income, "income")
                     };
                   <Transaction
                     txType
                     primary
                     amount=tx.amount
                     date=tx.date
                     onClick=(Router.clickToRoute(tx.detailsLink))
                     key=(string_of_int(iter + List.length(unconfirmed)))
                   />;
                 }),
            |]);
          },
        ),
      );
    <Grid
      ?warning
      title1=("Partners" |> text)
      title2=("Transactions" |> text)
      area1={
        <div>
          <MTypography variant=`Title>
            (viewData.ventureName |> text)
            <MaterialUi.IconButton
              className=(
                viewData.atRiskWarning ?
                  Styles.atRiskAddressButtonIcon : Styles.addressesButtonIcon
              )
              onClick=(
                Router.clickToRoute(Venture(viewData.ventureId, Addresses))
              )>
              (viewData.atRiskWarning ? Icons.alert : Icons.arrowUpCircle)
            </MaterialUi.IconButton>
          </MTypography>
          <Balance
            currentSpendable=viewData.balance.currentSpendable
            reserved=viewData.balance.reserved
          />
        </div>
      }
      area2={
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
      }
      area3={
        <div className=ScrollList.containerStyles>
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
          <ScrollList>
            <MaterialUi.List disablePadding=true> partners </MaterialUi.List>
          </ScrollList>
          <MButton
            fullWidth=true
            onClick=(
              Router.clickToRoute(
                Venture(viewData.ventureId, ManagePartners),
              )
            )>
            ("Add or Remove Partners" |> text)
          </MButton>
        </div>
      }
      area4={
        <div className=ScrollList.containerStyles>
          <ScrollList>
            <MaterialUi.List disablePadding=true> payouts </MaterialUi.List>
            <MaterialUi.List disablePadding=true>
              transactions
            </MaterialUi.List>
          </ScrollList>
        </div>
      }
    />;
  },
};
