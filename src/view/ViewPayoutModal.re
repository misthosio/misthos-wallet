include ViewCommon;

module ViewData = ViewModel.ViewPayoutView;

let component = ReasonReact.statelessComponent("Drawer");

let make =
    (~viewData: ViewData.t, ~commands: VentureWorkerClient.Cmd.t, _children) => {
  let voteStatus = (status: ViewData.voteStatus) =>
    (
      switch (status) {
      | Pending => "Pending"
      | Endorsed => "Endorsed"
      | Rejected => "Rejected"
      }
    )
    |> text;
  {
    ...component,
    render: _self => {
      let {
        processId,
        txId,
        date,
        status,
        summary,
        voters,
        canEndorse,
        canReject,
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
      let voters =
        ReasonReact.array(
          Array.of_list(
            voters
            |> List.map(({userId, voteStatus: status}: ViewData.voter) =>
                 <div>
                   <Partner partnerId=userId />
                   (status |> voteStatus)
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
        | Failed(errorMessage) =>
          "Failed with reason: " ++ errorMessage |> text
        };
      let transactionId =
        txId
        |> Utils.mapOption(txId => "Transaction ID: " ++ txId |> text)
        |> Js.Option.getWithDefault(ReasonReact.null);
      <Body2
        titles=["Payout Details"]
        body1=
          <div>
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
            <MTypography variant=`Title>
              ("Endorsements" |> text)
            </MTypography>
            <MaterialUi.List disablePadding=true> voters </MaterialUi.List>
            (
              if (canEndorse) {
                <MButton
                  fullWidth=true
                  onClick=(_e => commands.endorsePayout(~processId) |> ignore)>
                  (text("Endorse Payout"))
                </MButton>;
              } else {
                ReasonReact.null;
              }
            )
            (
              if (canReject) {
                <MButton
                  fullWidth=true
                  onClick=(_e => commands.rejectPayout(~processId) |> ignore)>
                  (text("Reject Payout"))
                </MButton>;
              } else {
                ReasonReact.null;
              }
            )
          </div>
      />;
    },
  };
};
