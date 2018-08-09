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
      backgroundColor(Colors.white),
      position(sticky),
      bottom(px(0)),
    ]);
  let noBorder = style([borderColor(`transparent)]);
  let link =
    style([color(Colors.black), hover([color(Colors.misthosTeal)])]);
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
      status: processStatus,
      canVote,
      data: {explorerLink, summary, payoutStatus: status, txId, date},
    }: ViewData.payout =
      viewData.payout;
    let executeEndorse = () => {
      open Js.Promise;
      let signatures =
        if (viewData.requiresLedgerSig) {
          commands.preSubmit(
            "Please confirm this endorsement on your ledger device (BTC app)",
          );
          viewData.signPayout();
        } else {
          resolve(Ledger.Signatures([||]));
        };
      signatures
      |> then_(
           fun
           | Ledger.Signatures(signatures) =>
             commands.endorsePayout(~processId, ~signatures) |> resolve
           | WrongDevice =>
             commands.preSubmitError(
               "The device does not have the correct seed for signing",
             )
             |> resolve
           | Error(Message(message)) =>
             commands.preSubmitError(message) |> resolve
           | Error(Unknown) =>
             commands.preSubmitError("An unknown error has occured")
             |> resolve,
         )
      |> ignore;
    };

    let destinationList =
      ReasonReact.array(
        Array.of_list(
          summary.destinations
          |> List.mapi((idx, (address, amount)) =>
               MaterialUi.(
                 <TableRow key=(idx |> string_of_int)>
                   <TableCell className=Styles.noBorder padding=`None>
                     <MTypography variant=`Body2>
                       (address |> text)
                     </MTypography>
                   </TableCell>
                   <TableCell
                     numeric=true className=Styles.noBorder padding=`None>
                     <MTypography variant=`Body2>
                       (BTC.format(amount) ++ " BTC" |> text)
                     </MTypography>
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
    <Grid
      title1=("Payout Details" |> text)
      area3={
        <div className=ScrollList.containerStyles>
          <MTypography variant=`Body2 gutterBottom=true>
            (
              switch (date) {
              | Some(date) =>
                "Payout completed on " ++ Js.Date.toDateString(date) |> text
              | None => "Proposed by " ++ UserId.toString(proposedBy) |> text
              }
            )
          </MTypography>
          <MTypography variant=`Body2>
            ("Status: " |> text)
            payoutStatus
          </MTypography>
          <MTypography variant=`Title gutterTop=true>
            ("Payout" |> text)
          </MTypography>
          <ScrollList>
            MaterialUi.(
              <Table>
                <TableBody>
                  destinationList
                  <TableRow key="networkFee">
                    <TableCell className=Styles.noBorder padding=`None>
                      <MTypography variant=`Body2>
                        ("NETWORK FEE" |> text)
                      </MTypography>
                    </TableCell>
                    <TableCell
                      numeric=true className=Styles.noBorder padding=`None>
                      <MTypography variant=`Body2>
                        (BTC.format(summary.networkFee) ++ " BTC" |> text)
                      </MTypography>
                    </TableCell>
                  </TableRow>
                  <TableRow key="misthosFee">
                    <TableCell className=Styles.noBorder padding=`None>
                      <MTypography variant=`Body2>
                        ("MISTHOS FEE" |> text)
                      </MTypography>
                    </TableCell>
                    <TableCell
                      numeric=true className=Styles.noBorder padding=`None>
                      <MTypography variant=`Body2>
                        (BTC.format(summary.misthosFee) ++ " BTC" |> text)
                      </MTypography>
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
          </ScrollList>
        </div>
      }
      area4={
        <div className=ScrollList.containerStyles>
          <Voters
            voters
            currentPartners=viewData.currentPartners
            processStatus
          />
          <ProcessApprovalButtons
            endorseText="Endorse Payout"
            rejectText="Reject Payout"
            canVote
            onEndorse=executeEndorse
            onReject=(() => commands.rejectPayout(~processId))
            onCancel=(() => commands.reset())
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
      }
      area5=(
        switch (txId, explorerLink) {
        | (Some(txId), Some(explorerLink)) =>
          <div>
            <MTypography variant=`Title>
              ("Transaction ID" |> text)
            </MTypography>
            <MTypography variant=`Body2>
              <a className=Styles.link href=explorerLink target="_blank">
                (txId |> text)
              </a>
            </MTypography>
          </div>
        | _ => <MTypography variant=`Body1> PolicyText.payout </MTypography>
        }
      )
    />;
  },
};
