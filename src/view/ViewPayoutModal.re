open PrimitiveTypes;

include ViewCommon;

module ViewData = ViewModel.ViewPayoutView;

let component = ReasonReact.statelessComponent("ViewPayoutModal");

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
    }: ViewData.t = viewData;
    let destinationList =
      ReasonReact.array(
        Array.of_list(
          summary.destinations
          |> List.mapi((idx, (address, amount)) =>
               <div key=(idx |> string_of_int)>
                 (text(address ++ " - " ++ BTC.format(amount)))
               </div>
             ),
        ),
      );
    let payoutStatus = {
      let (label, status: StatusChip.status) =
        switch (status) {
        | PendingApproval => ("Pending Approval", Pending)
        | Accepted => ("Accepted", Success)
        | Denied => ("Denied", Failure)
        | Unconfirmed => ("Unconfirmed", Pending)
        | Confirmed => ("Confirmed", Success)
        | Failed(_) => ("Failed", Failure)
        };
      <StatusChip label status />;
    };
    let transactionId =
      txId
      |> Utils.mapOption(txId => "Transaction ID: " ++ txId |> text)
      |> Js.Option.getWithDefault(ReasonReact.null);
    <Body2
      titles=["Payout Details"]
      body1=
        <div>
          ("Proposed by " ++ UserId.toString(proposedBy) |> text)
          (
            date
            |> Utils.mapOption(date => Js.Date.toString(date) |> text)
            |> Js.Option.getWithDefault(ReasonReact.null)
          )
          <MTypography variant=`Body2>
            ("Status: " |> text)
            payoutStatus
          </MTypography>
          <MTypography variant=`Title> ("Payout" |> text) </MTypography>
          <ul>
            destinationList
            <li>
              (text("Network Fee - " ++ BTC.format(summary.networkFee)))
            </li>
            <li>
              (text("Misthos Fee - " ++ BTC.format(summary.misthosFee)))
            </li>
            <li>
              (text("Total payout- " ++ BTC.format(summary.spentWithFees)))
            </li>
          </ul>
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
        </div>
    />;
  },
};
