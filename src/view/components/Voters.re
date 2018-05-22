include ViewCommon;

let component = ReasonReact.statelessComponent("Voters");

let make = (~voters: list(ProcessCollector.voter), _children) => {
  ...component,
  render: (_) => {
    let voteStatus = (status: ProcessCollector.voteStatus) =>
      (
        switch (status) {
        | Pending => "Pending"
        | Endorsed => "Endorsed"
        | Rejected => "Rejected"
        }
      )
      |> text;
    let voters =
      ReasonReact.array(
        Array.of_list(
          voters
          |> List.map(({userId, voteStatus: status}: ProcessCollector.voter) =>
               <div> <Partner partnerId=userId /> (status |> voteStatus) </div>
             ),
        ),
      );
    ReasonReact.array([|
      <MTypography variant=`Title>
        ("Endorsement Status" |> text)
      </MTypography>,
      <MaterialUi.List disablePadding=true> voters </MaterialUi.List>,
    |]);
  },
};
