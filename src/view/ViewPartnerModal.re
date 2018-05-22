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
  render: (_) => {
    let {processId, voters, canVote, status, data: {userId, processType}}: ViewData.t = viewData;
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
    <Body2
      titles=["Proposed Partner " ++ processTypeString]
      body1=
        <div>
          (
            "Status: "
            ++ (
              switch (status) {
              | PendingApproval => "PendingApproval"
              | Accepted => "Accepted"
              | Rejected => "Rejected"
              | Aborted => "Aborted"
              }
            )
            |> text
          )
          <MTypography variant=`Title>
            ("Proposed Partner " ++ processTypeString |> text)
          </MTypography>
          <Partner key=(userId |> UserId.toString) partnerId=userId />
        </div>
      body2=
        <div>
          <Voters voters />
          <ProcessApprovalButtons
            endorseText=("Endorse Partner " ++ processTypeString)
            rejectText=("Reject Partner " ++ processTypeString)
            canVote
            onEndorse
            onReject
            cmdStatus
          />
        </div>
    />;
  },
};
