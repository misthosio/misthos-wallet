include ViewCommon;

module ViewData = ViewModel.SelectedVentureView;

open PrimitiveTypes;

let component = ReasonReact.statelessComponent("SelectedVenture");

let make = (~viewData: ViewData.t, _children) => {
  ...component,
  render: (_) => {
    let prospects =
      viewData.prospects
      |> List.map((prospect: ViewData.prospect) =>
           MaterialUi.(
             <ListItem
               button=true
               onClick=(
                 Router.clickToRoute(
                   Venture(viewData.ventureId, Partner(prospect.userId)),
                 )
               )
               key=(prospect.processId |> ProcessId.toString)>
               (
                 text(
                   (
                     switch (prospect.processType) {
                     | Removal => "Removal"
                     | Addition => "Addition"
                     }
                   )
                   ++ " of '"
                   ++ UserId.toString(prospect.userId)
                   ++ "' proposed",
                 )
               )
             </ListItem>
           )
         );
    let partners =
      ReasonReact.array(
        Array.of_list(
          Belt.List.concat(
            prospects,
            viewData.partners
            |> List.map((partner: ViewData.partner) =>
                 <Partner
                   key=(partner.userId |> UserId.toString)
                   partnerId=partner.userId
                   name=?partner.name
                 />
               ),
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
