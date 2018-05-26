open PrimitiveTypes;

include ViewCommon;

module ViewData = ViewModel.ViewPayoutView;

let component = ReasonReact.statelessComponent("ViewPayoutModal");

module Styles = {
  open Css;
  let total =
    style([
      display(`flex),
      justifyContent(spaceBetween),
      alignItems(`baseline),
    ]);
  let noBorder = style([borderColor(`transparent)]);
};

let make =
    (
      ~viewData: ViewData.t,
      ~commands: CommandExecutor.commands,
      ~cmdStatus,
      _children,
    ) => {
  ...component,
  render: _self => {
    let {
      proposedBy,
      processId,
      voters,
      canVote,
      data: {summary, payoutStatus: status, txId, date},
    }: ViewData.payout =
      viewData.payout;
    let destinationList =
      ReasonReact.array(
        Array.of_list(
          summary.destinations
          |> List.mapi((idx, (address, amount)) =>
               MaterialUi.(
                 <TableRow key=(idx |> string_of_int)>
                   <TableCell className=Styles.noBorder padding=`None>
                     <b> (address |> text) </b>
                   </TableCell>
                   <TableCell
                     numeric=true className=Styles.noBorder padding=`None>
                     (BTC.format(amount) ++ " BTC" |> text)
                   </TableCell>
                 </TableRow>
               )
             ),
        ),
      );
    let payoutStatus = {
      let (label, status: StatusChip.status) =
        switch (status) {
        | PendingApproval => ("Pending Approval", Pending)
        | Accepted => ("Accepted", Success)
        | Denied => ("Denied", Failure)
        | Aborted => ("Aborted", Failure)
        | Unconfirmed => ("Unconfirmed", Pending)
        | Confirmed => ("Confirmed", Success)
        | Failed(_) => ("Failed", Failure)
        };
      <StatusChip label status />;
    };
    let transactionId =
      switch (txId) {
      | Some(txId) =>
        <div>
          <MTypography variant=`Title>
            ("Transaction ID" |> text)
          </MTypography>
          <MTypography variant=`Body2> (txId |> text) </MTypography>
        </div>
      | None => ReasonReact.null
      };
    <Body2
      titles=["Payout Details"]
      body1=
        <div>
          <MTypography variant=`Body2>
            (
              switch (date) {
              | Some(date) =>
                "Payout completed on " ++ Js.Date.toString(date) |> text
              | None => "Proposed by " ++ UserId.toString(proposedBy) |> text
              }
            )
          </MTypography>
          <MTypography variant=`Body2>
            ("Status: " |> text)
            payoutStatus
          </MTypography>
          <MTypography variant=`Title> ("Payout" |> text) </MTypography>
          MaterialUi.(
            <Table>
              <TableBody>
                destinationList
                <TableRow key="networkFee">
                  <TableCell className=Styles.noBorder padding=`None>
                    <b> ("NETWORK FEE" |> text) </b>
                  </TableCell>
                  <TableCell
                    numeric=true className=Styles.noBorder padding=`None>
                    (BTC.format(summary.networkFee) ++ " BTC" |> text)
                  </TableCell>
                </TableRow>
                <TableRow key="misthosFee">
                  <TableCell className=Styles.noBorder padding=`None>
                    <b> ("MISTHOS FEE" |> text) </b>
                  </TableCell>
                  <TableCell
                    numeric=true className=Styles.noBorder padding=`None>
                    (BTC.format(summary.misthosFee) ++ " BTC" |> text)
                  </TableCell>
                </TableRow>
              </TableBody>
            </Table>
          )
          MaterialUi.(
            <div className=Styles.total>
              <Typography variant=`Body2>
                ("TOTAL PAYOUT" |> text)
              </Typography>
              <MTypography className=Styles.total variant=`Subheading>
                (BTC.format(summary.spentWithFees) ++ " BTC" |> text)
              </MTypography>
            </div>
          )
          transactionId
        </div>
      body2=
        <div>
          <Voters voters />
          <ProcessApprovalButtons
            endorseText="Endorse Payout"
            rejectText="Reject Payout"
            canVote
            onEndorse=(() => commands.endorsePayout(~processId))
            onReject=(() => commands.rejectPayout(~processId))
            cmdStatus
          />
          (
            if (viewData.collidesWith |> Belt.Set.size > 0) {
              <MaterialUi.SnackbarContent
                message=(
                  {|
                   This Proposal is reusing inputs reserved by another payout.
                   We recommend that you coordinate with your Partners
                   to only endorse one Proposal and reject the other one.
                   |}
                  |> text
                )
              />;
            } else {
              ReasonReact.null;
            }
          )
        </div>
    />;
  },
};
