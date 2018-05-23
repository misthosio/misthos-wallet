open PrimitiveTypes;

include ViewCommon;

module ViewData = ViewModel.ViewPayoutView;

let component = ReasonReact.statelessComponent("Drawer");

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
    let payoutStatus =
      switch (status) {
      | PendingApproval => "PendingApproval" |> text
      | Accepted => "Accepted" |> text
      | Unconfirmed => "Unconfirmed" |> text
      | Confirmed => "Confirmed" |> text
      | Failed(errorMessage) => "Failed with reason: " ++ errorMessage |> text
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
          payoutStatus
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
