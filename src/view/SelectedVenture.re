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
           <AlertListItem
             icon=(
               switch (prospect.data.processType) {
               | Removal => Minus
               | Addition => Plus
               }
             )
             onClick=(
               Router.clickToRoute(
                 Venture(viewData.ventureId, Partner(prospect.processId)),
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
               text("proposed by " ++ UserId.toString(prospect.proposedBy))
             )
           />
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
      ReasonReact.array(
        Array.of_list(
          viewData.payoutsPendingApproval
          |> List.map(
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
        Array.of_list(
          {
            let unconfirmed = viewData.unconfirmedTxs;
            let confirmed = viewData.confirmedTxs;
            Belt.List.concatMany([|
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
    <Grid
      title1=("Partners" |> text)
      title2=("Transactions" |> text)
      area1=
        <div>
          <MTypography variant=`Title>
            (viewData.ventureName |> text)
          </MTypography>
          <Balance
            currentSpendable=viewData.balance.currentSpendable
            reserved=viewData.balance.reserved
          />
        </div>
      area2=
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
      area3=
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
          <ScrollList>
            <MaterialUi.List disablePadding=true> partners </MaterialUi.List>
          </ScrollList>
          <LinkButton
            fullWidth=true
            route=(Venture(viewData.ventureId, ManagePartners))>
            ("Add or Remove Partners" |> text)
          </LinkButton>
        </div>
      area4=MaterialUi.(
              <div>
                <ScrollList>
                  <List disablePadding=true> payouts </List>
                  <List disablePadding=true> transactions </List>
                </ScrollList>
              </div>
            )
    />;
  },
};
