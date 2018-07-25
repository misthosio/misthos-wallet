include ViewCommon;

open PrimitiveTypes;

module ViewData = ViewModel.ViewPartnerView;

type state = {
  viewData: ViewData.t,
  loggedInStatus: option(bool),
};

type action =
  | SetHasLoggedIn(bool);

type onboardingState =
  | SignInRequired
  | PendingApproval
  | SyncRequired
  | FullyOnboarded
  | None;

let component = ReasonReact.reducerComponent("ViewPartner");

let updateLoggedInStatus = (partnerProcess: ViewData.partnerProcess, send) =>
  Js.Promise.(
    partnerProcess.data.hasLoggedIn
    |> then_(known => send(SetHasLoggedIn(known)) |> resolve)
  )
  |> ignore;

let make =
    (
      ~viewData: ViewData.t,
      ~commands: CommandExecutor.commands,
      ~cmdStatus: CommandExecutor.cmdStatus,
      _children,
    ) => {
  ...component,
  initialState: () => {viewData, loggedInStatus: None},
  willReceiveProps: ({state, send}) => {
    updateLoggedInStatus(viewData.partnerProcess, send);
    {viewData, loggedInStatus: state.loggedInStatus};
  },
  reducer: (action, state) =>
    switch (action) {
    | SetHasLoggedIn(known) =>
      ReasonReact.Update({...state, loggedInStatus: Some(known)})
    },
  didMount: ({send}) => updateLoggedInStatus(viewData.partnerProcess, send),
  render: ({state: {viewData, loggedInStatus}}) => {
    let {
      proposedBy,
      processId,
      voters,
      canVote,
      status,
      data: {userId, processType},
    }: ViewData.partnerProcess =
      viewData.partnerProcess;
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
    let onboardingState =
      switch (processType) {
      | Addition =>
        switch (
          status,
          viewData.partnerProcess.data.joinedWallet,
          loggedInStatus,
        ) {
        | (Accepted, true, _) => FullyOnboarded
        | (PendingApproval | Accepted, false, Some(false)) => SignInRequired
        | (PendingApproval, _, _) => PendingApproval
        | (Accepted, false, Some(true)) => SyncRequired
        | (Accepted, false, None) => SyncRequired
        | (Aborted | Denied, _, _) => None
        }
      | Removal => None
      };
    let onboardingStatusChip =
      switch (onboardingState) {
      | SignInRequired =>
        <StatusChip status=Pending label="SIGN IN REQUIRED" />
      | PendingApproval =>
        <StatusChip status=Pending label="PENDING APPROVAL" />
      | SyncRequired => <StatusChip status=Pending label="SYNC REQUIRED" />
      | FullyOnboarded => <StatusChip status=Success label="ONBOARDED" />
      | None => ReasonReact.null
      };
    let onboardingBody =
      switch (onboardingState) {
      | SignInRequired =>
        <AlertBox>
          <MTypography variant=`Body1>
            (
              (viewData.partnerProcess.data.userId |> UserId.toString)
              ++ {| has not yet signed into Misthos and is therefore missing a
                    public key. Please remind them to sign in to automatically
                    expose a public key before joining the Venture.|}
              |> text
            )
          </MTypography>
        </AlertBox>
      | PendingApproval =>
        <MTypography variant=`Body1>
          (
            (viewData.partnerProcess.data.userId |> UserId.toString)
            ++ {| has to be fully endorsed before onboarding can proceed.|}
            |> text
          )
        </MTypography>
      | SyncRequired =>
        <AlertBox>
          <MTypography variant=`Body1>
            (
              (viewData.partnerProcess.data.userId |> UserId.toString)
              ++ {| has been fully endorsed and is ready to sync data with the
                    Venture. Please send them the Venture sync URL to complete
                    the process.|}
              |> text
            )
          </MTypography>
        </AlertBox>
      | FullyOnboarded =>
        <MTypography variant=`Body1>
          (
            (viewData.partnerProcess.data.userId |> UserId.toString)
            ++ {| is fully onboarded to this Venture.|}
            |> text
          )
        </MTypography>
      | None => ReasonReact.null
      };
    let onboarding =
      switch (onboardingState) {
      | None => ReasonReact.null
      | _ =>
        [|
          <MTypography gutterTop=true gutterBottom=true variant=`Title>
            ("Partner Onboarding" |> text)
          </MTypography>,
          <MTypography gutterBottom=true variant=`Body2>
            ("Status: " |> text)
            onboardingStatusChip
          </MTypography>,
          onboardingBody,
        |]
        |> ReasonReact.array
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
    let alertText =
      viewData.atRiskWarning ? WarningsText.partnerRemovalRisk |. Some : None;
    <Grid
      title1=("Proposed Partner " ++ processTypeString |> text)
      area3={
        <div>
          <MTypography variant=`Title>
            ("Proposed Partner " ++ processTypeString |> text)
          </MTypography>
          <Partner key=(userId |> UserId.toString) partnerId=userId />
          <MTypography variant=`Body2 gutterBottom=true>
            ("Proposed by " ++ UserId.toString(proposedBy) |> text)
          </MTypography>
          <MTypography variant=`Body2>
            ("Status: " |> text)
            statusChip
          </MTypography>
          onboarding
        </div>
      }
      area4={
        <div>
          <Voters voters />
          <ProcessApprovalButtons
            endorseText=("Endorse Partner " ++ processTypeString)
            rejectText=("Reject Partner " ++ processTypeString)
            ?alertText
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
