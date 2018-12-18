open Belt;

include ViewCommon;

let component = ReasonReact.statelessComponent("Voters");

let make =
    (
      ~currentPartners,
      ~voters: list(ProcessCollector.voter),
      ~processStatus,
      _children,
    ) => {
  ...component,
  render: _ => {
    let voters =
      ReasonReact.array(
        List.toArray(
          voters->(
                    List.map(
                      ({userId, voteStatus: status}: ProcessCollector.voter) => {
                      let (label, status: option(StatusChip.status), ex) =
                        switch (
                          currentPartners->(Set.has(userId)),
                          status,
                          processStatus,
                        ) {
                        | (false, Pending, _) => (
                            "Didn't Vote",
                            Some(Neutral),
                            true,
                          )
                        | (false, _, _) => (
                            "Discounted",
                            Some(Neutral),
                            true,
                          )
                        | (_, Pending, ProcessCollector.PendingApproval) => (
                            "Pending",
                            Some(Pending),
                            false,
                          )
                        | (_, Pending, _) => (
                            "Didn't Vote",
                            Some(Neutral),
                            false,
                          )
                        | (_, Endorsed, _) => (
                            "Endorsed",
                            Some(Success),
                            false,
                          )
                        | (_, Rejected, _) => (
                            "Rejected",
                            Some(Failure),
                            false,
                          )
                        };
                      <Partner
                        partnerId=userId
                        status={
                          status
                          |> Utils.mapOption(status =>
                               <StatusChip label status />
                             )
                        }
                        ex
                      />;
                    })
                  ),
        ),
      );
    ReasonReact.array([|
      <MTypography variant=`Title>
        {"Endorsement Status" |> text}
      </MTypography>,
      <ScrollList>
        <MaterialUi.List disablePadding=true> voters </MaterialUi.List>
      </ScrollList>,
    |]);
  },
};
