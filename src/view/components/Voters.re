include ViewCommon;

let component = ReasonReact.statelessComponent("Voters");

let make = (~voters: list(ProcessCollector.voter), _children) => {
  ...component,
  render: _ => {
    let voters =
      ReasonReact.array(
        Array.of_list(
          voters
          |> List.map(({userId, voteStatus: status}: ProcessCollector.voter) => {
               let (label, status: StatusChip.status) =
                 switch (status) {
                 | Pending => ("Pending", Pending)
                 | Endorsed => ("Endorsed", Success)
                 | Rejected => ("Rejected", Failure)
                 };
               <Partner
                 partnerId=userId
                 button={<StatusChip label status />}
               />;
             }),
        ),
      );
    ReasonReact.array([|
      <MTypography variant=`Title>
        ("Endorsement Status" |> text)
      </MTypography>,
      <ScrollList>
        <MaterialUi.List disablePadding=true> voters </MaterialUi.List>
      </ScrollList>,
    |]);
  },
};
