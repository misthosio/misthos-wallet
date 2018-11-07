open Belt;

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
  let noBorder = style([borderColor(`transparent), maxWidth(vw(50.0))]);
  let link =
    style([color(Colors.black), hover([color(Colors.misthosTeal)])]);
  let ellipsis = style([textOverflow(ellipsis), overflow(hidden)]);
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
      data: {
        payoutTx,
        explorerLink,
        summary,
        payoutStatus: status,
        txId,
        date,
      },
    }: ViewData.payout =
      viewData.payout;
    let executeCommand = (~justSign=false, ()) => {
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
             (
               justSign ?
                 commands.signPayout(~processId, ~signatures) :
                 commands.endorsePayout(~processId, ~signatures)
             )
             |> resolve
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
        List.toArray(
          summary.destinations
          |. List.mapWithIndexU((. idx, (address, amount)) =>
               MaterialUi.(
                 <TableRow key=(idx |> string_of_int)>
                   <TableCell className=Styles.noBorder padding=`None>
                     <MTypography className=Styles.ellipsis variant=`Body2>
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
    let pendingSignatures =
      status == Accepted && viewData.missingSignatures |. Set.size > 0;

    switch (cmdStatus) {
    | CommandExecutor.PreSubmit(_) =>
      <LedgerConfirmation
        action=CommandExecutor.Status.Endorsement
        onCancel=(() => commands.reset())
        summary
        misthosFeeAddress=(Some(payoutTx.misthosFeeAddress))
        changeAddress=payoutTx.changeAddress
        cmdStatus
      />

    | _ =>
      <Grid
        title1=("Payout Details" |> text)
        area3={
          <div className=ScrollList.containerStyles>
            <MTypography variant=`Body2 gutterBottom=true>
              (
                switch (date) {
                | Some(date) =>
                  "Payout completed on " ++ Js.Date.toDateString(date) |> text
                | None =>
                  "Proposed by " ++ UserId.toString(proposedBy) |> text
                }
              )
            </MTypography>
            <MTypography variant=`Body2>
              ("Status: " |> text)
              payoutStatus
            </MTypography>
            (
              switch (pendingSignatures) {
              | true =>
                <MTypography variant=`Body2>
                  ("Transaction Status: " |> text)
                  <StatusChip label="Pending" status=Pending />
                </MTypography>
              | _ => ReasonReact.null
              }
            )
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
                    (
                      if (summary.misthosFee |> BTC.gt(BTC.zero)) {
                        <TableRow key="misthosFee">
                          <TableCell className=Styles.noBorder padding=`None>
                            <MTypography variant=`Body2>
                              ("MISTHOS FEE" |> text)
                            </MTypography>
                          </TableCell>
                          <TableCell
                            numeric=true
                            className=Styles.noBorder
                            padding=`None>
                            <MTypography variant=`Body2>
                              (
                                BTC.format(summary.misthosFee)
                                ++ " BTC"
                                |> text
                              )
                            </MTypography>
                          </TableCell>
                        </TableRow>;
                      } else {
                        ReasonReact.null;
                      }
                    )
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
        area4=(
          switch (pendingSignatures) {
          | false =>
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
                onEndorse=executeCommand
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
          | _ =>
            <div className=ScrollList.containerStyles>
              <MTypography variant=`Title>
                ("Pending Signatures" |> text)
              </MTypography>
              <MTypography variant=`Body2>
                (
                  "Additional signatures are required by the bitcoin network in order for this transaction to proceed. The following custodians have yet to sign this transaction:"
                  |> text
                )
              </MTypography>
              <ScrollList>
                <MaterialUi.List disablePadding=true>
                  (
                    ReasonReact.array(
                      viewData.missingSignatures
                      |. Set.toArray
                      |. Array.mapU((. userId) =>
                           <Partner
                             partnerId=userId
                             status={
                               <StatusChip label="Pending" status=Pending />
                             }
                           />
                         ),
                    )
                  )
                </MaterialUi.List>
              </ScrollList>
              (
                switch (
                  viewData.missingSignatures |. Set.has(viewData.localUser)
                ) {
                | true =>
                  <SingleActionButton
                    onSubmit=(executeCommand(~justSign=true))
                    canSubmitAction=true
                    withConfirmation=false
                    action=CommandExecutor.Status.SignTransaction
                    buttonText="Sign Transaction"
                    cmdStatus
                  />
                | _ => ReasonReact.null
                }
              )
              (
                if (viewData.collidesWith |> Belt.Set.size > 0) {
                  <MaterialUi.SnackbarContent
                    message=(
                      {|
                   This Proposal is reusing inputs reserved by another payout.
                   We recommend that you coordinate with your Partners
                   to only sign one Proposal.
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
        )
        area5=(
          switch (txId, explorerLink) {
          | (Some(txId), Some(explorerLink)) =>
            <div>
              <MTypography variant=`Title>
                ("Transaction ID" |> text)
              </MTypography>
              <MTypography className=Styles.ellipsis variant=`Body2>
                <a className=Styles.link href=explorerLink target="_blank">
                  (txId |> text)
                </a>
              </MTypography>
            </div>
          | _ => ReasonReact.null
          }
        )
      />
    };
  },
};
