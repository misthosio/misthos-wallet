include ViewCommon;

open PrimitiveTypes;

open WalletTypes;

type action =
  | CommandExecuted(WebWorker.correlationId);

type commands = {
  proposePartner: (~prospectId: userId) => unit,
  endorsePartner: (~processId: processId) => unit,
  rejectPartner: (~processId: processId) => unit,
  proposePartnerRemoval: (~partnerId: userId) => unit,
  endorsePartnerRemoval: (~processId: processId) => unit,
  rejectPartnerRemoval: (~processId: processId) => unit,
  proposePayout:
    (
      ~accountIdx: accountIdx,
      ~destinations: list((string, BTC.t)),
      ~fee: BTC.t
    ) =>
    unit,
  endorsePayout: (~processId: processId) => unit,
  rejectPayout: (~processId: processId) => unit,
};

type cmdStatus =
  | Idle
  | Pending(WebWorker.correlationId)
  | Error(VentureWorkerMessage.cmdError)
  | Success(VentureWorkerMessage.cmdSuccess);

type state = {cmdStatus};

let component = ReasonReact.reducerComponent("CommandExecuter");

let make =
    (
      ~commands: VentureWorkerClient.Cmd.t,
      ~lastResponse,
      ~onProcessStarted=?,
      children,
    ) => {
  let wrapCommands = send => {
    proposePartner: (~prospectId) =>
      send(CommandExecuted(commands.proposePartner(~prospectId))),
    endorsePartner: (~processId) =>
      send(CommandExecuted(commands.endorsePartner(~processId))),
    rejectPartner: (~processId) =>
      send(CommandExecuted(commands.rejectPartner(~processId))),
    proposePartnerRemoval: (~partnerId: userId) =>
      send(CommandExecuted(commands.proposePartnerRemoval(~partnerId))),
    endorsePartnerRemoval: (~processId) =>
      send(CommandExecuted(commands.endorsePartnerRemoval(~processId))),
    rejectPartnerRemoval: (~processId) =>
      send(CommandExecuted(commands.rejectPartnerRemoval(~processId))),
    proposePayout: (~accountIdx, ~destinations, ~fee) =>
      send(
        CommandExecuted(
          commands.proposePayout(~accountIdx, ~destinations, ~fee),
        ),
      ),
    endorsePayout: (~processId) =>
      send(CommandExecuted(commands.endorsePayout(~processId))),
    rejectPayout: (~processId) =>
      send(CommandExecuted(commands.rejectPayout(~processId))),
  };
  {
    ...component,
    initialState: () => {cmdStatus: Idle},
    willReceiveProps: ({state: {cmdStatus}}) => {
      cmdStatus:
        switch (cmdStatus, lastResponse) {
        | (Pending(correlationId), Some((responseId, response)))
            when correlationId == responseId =>
          switch ((response: VentureWorkerMessage.cmdResponse)) {
          | Ok(ProcessStarted(processId)) =>
            onProcessStarted |> Utils.mapOption(fn => fn(processId)) |> ignore;
            Success(ProcessStarted(processId));
          | Ok(success) => Success(success)
          | Error(err) => Error(err)
          }
        | _ => cmdStatus
        },
    },
    reducer: (action, _state) =>
      switch (action) {
      | CommandExecuted(correlationId) =>
        ReasonReact.Update({cmdStatus: Pending(correlationId)})
      },
    render: ({send, state: {cmdStatus}}) =>
      children(~commands=wrapCommands(send), ~cmdStatus),
  };
};

module Status = {
  type action =
    | CreateVenture
    | JoinVenture
    | LoadVenture
    | Proposal
    | Endorsement
    | Rejection;
  let component = ReasonReact.statelessComponent("CommandStatus");
  let make = (~cmdStatus, ~action, ~onRetry=?, _children) => {
    ...component,
    render: (_) =>
      switch (cmdStatus) {
      | Idle => ReasonReact.null
      | Pending(_) =>
        ReasonReact.array([|
          <MTypography variant=`Body2>
            (
              (
                switch (action) {
                | CreateVenture => "Venture is being created"
                | JoinVenture => "Joining venture"
                | LoadVenture => "Loading venture"
                | Proposal => "Your proposal is being submitted"
                | Endorsement => "Your endorsement is being submitted"
                | Rejection => "Your rejection is being submitted"
                }
              )
              |> text
            )
          </MTypography>,
          <MaterialUi.LinearProgress
            className=Css.(
                        style([
                          marginTop(px(Theme.space(1))),
                          backgroundColor(Colors.misthosTeal),
                        ])
                      )
          />,
        |])
      | Error(error) =>
        switch (error, onRetry) {
        | (CouldNotPersistVenture, Some(onRetry)) =>
          ReasonReact.array([|
            "RED: your submission could not be persisted" |> text,
            <MButton fullWidth=true onClick=(_e => onRetry())>
              (text("Try Again"))
            </MButton>,
          |])
        | (CouldNotPersistVenture, None) =>
          "RED: your submission could not be persisted" |> text
        | (CouldNotFindUserInfo, _) =>
          "RED: Id doesn't exist or user has never logged in" |> text
        | (MaxPartnersReached, _) =>
          "RED: The maximum number of partners we currently support in a venture has been reached"
          |> text
        | (PartnerAlreadyProposed, _) =>
          "RED: This user has already been proposed to join" |> text
        | (PartnerAlreadyExists, _) =>
          "RED: User is already a partner of this venture" |> text
        | (CouldNotJoinVenture, _) =>
          "RED: Error joining venture. Perhaps you have not been accepted yet."
          |> text
        | (CouldNotLoadVenture, _) => "RED: Error loading venture" |> text
        }
      | Success(ProcessStarted(_)) =>
        "GREEN: Your proposal has been submited" |> text
      | Success(ProcessEndorsed(_)) =>
        "GREEN: Your endorsement has been submited" |> text
      | Success(ProcessRejected(_)) =>
        "GREEN: Your rejection has been submited" |> text
      },
  };
};
