include ViewCommon;

open PrimitiveTypes;

[@bs.module] external remove : string = "../assets/img/remove-partner.svg";

module ViewData = ViewModel.ViewPartnerView;

type state =
  | NoDecision
  | ConfirmEndorse
  | ConfirmReject;

type action =
  | Endorse
  | Reject;

let component = ReasonReact.statelessComponent("ViewPartner");

let make =
    (
      ~viewData: ViewData.t,
      ~commands: CommandExecutor.commands,
      ~cmdStatus: CommandExecutor.cmdStatus,
      _children,
    ) => {
  ...component,
  render: _ => {
    let {
      proposedBy,
      processId,
      voters,
      canVote,
      status,
      data: {userId, processType},
    }: ViewData.t = viewData;
    let (onEndorse, onReject) =
      switch (processType) {
      | Addition => (
          (() => commands.endorsePartner(~processId)),
          (() => commands.rejectPartner(~processId)),
        )
      | Removal => (
          (() => commands.endorsePartnerRemoval(~processId)),
          (() => commands.rejectPartnerRemoval(~processId)),
        )
      };
    let processTypeString =
      switch (processType) {
      | Addition => "Addition"
      | Removal => "Removal"
      };
    let statusChip = {
      let (label, status: StatusChip.status) =
        switch (status) {
        | PendingApproval => ("Pending Approval", Pending)
        | Accepted => ("Accepted", Success)
        | Aborted => ("Aborted", Failure)
        | Denied => ("Denied", Failure)
        };
      <StatusChip status label />;
    };
    <Body2
      titles=["Proposed Partner " ++ processTypeString]
      body1={
        <div>
          <MTypography variant=`Title>
            ("Proposed Partner " ++ processTypeString |> text)
          </MTypography>
          <Partner key=(userId |> UserId.toString) partnerId=userId />
          <MTypography variant=`Body2>
            ("Proposed by " ++ UserId.toString(proposedBy) |> text)
          </MTypography>
          <MTypography variant=`Body2>
            ("Status: " |> text)
            statusChip
          </MTypography>
        </div>
      }
      body2={
        <div>
          <Voters voters />
          <ProcessApprovalButtons
            endorseText=("Endorse Partner " ++ processTypeString)
            rejectText=("Reject Partner " ++ processTypeString)
            canVote
            onEndorse
            onReject
            onCancel=(() => commands.reset())
            cmdStatus
          />
        </div>
      }
    />;
  },
};
