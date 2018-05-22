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

let component = ReasonReact.statelessComponent("ManagePartners");

let make =
    (
      ~viewData: ViewData.t,
      ~commands: CommandExecutor.commands,
      ~cmdStatus: CommandExecutor.cmdStatus,
      _children,
    ) => {
  ...component,
  render: (_) => {
    let {processId, userId, voters, canEndorse, canReject, processType}: ViewData.t = viewData;
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
    let voteStatus = (status: ViewData.voteStatus) =>
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
          |> List.map(({userId, voteStatus: status}: ViewData.voter) =>
               <div> <Partner partnerId=userId /> (status |> voteStatus) </div>
             ),
        ),
      );
    <Body2
      titles=["Proposed Partner " ++ processTypeString]
      body1=
        <div>
          ("Status: Pending" |> text)
          <MTypography variant=`Title>
            ("Proposed Partner " ++ processTypeString |> text)
          </MTypography>
          <Partner key=(userId |> UserId.toString) partnerId=userId />
        </div>
      body2=
        <div>
          <MTypography variant=`Title>
            ("Endorsement Status" |> text)
          </MTypography>
          <MaterialUi.List disablePadding=true> voters </MaterialUi.List>
          <ProcessApprovalButtons
            endorseText=("Endorse Partner " ++ processTypeString)
            rejectText=("Reject Partner " ++ processTypeString)
            canEndorse
            canReject
            onEndorse
            onReject
            cmdStatus
          />
        </div>
    />;
  },
};
