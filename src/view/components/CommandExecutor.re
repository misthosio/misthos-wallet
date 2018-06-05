include ViewCommon;

open PrimitiveTypes;

open WalletTypes;

type action =
  | CommandExecuted(WebWorker.correlationId)
  | Reset;

type commands = {
  reset: unit => unit,
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
    reset: () => send(Reset),
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
      | Reset => ReasonReact.Update({cmdStatus: Idle})
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
  type reducerAction =
    | Success
    | Error;
  type state = {
    fired: bool,
    cmdStatus,
  };
  let message = (variant: reducerAction, message: string) => {
    let color =
      switch (variant) {
      | Error => Colors.error
      | Success => Colors.success
      };
    <MTypography variant=`Body2 className=(Css.style([Css.color(color)]))>
      (message |> text)
    </MTypography>;
  };
  let component = ReasonReact.statelessComponent("CommandStatus");
  let make = (~cmdStatus: cmdStatus, ~action, _children) => {
    ...component,
    render: _ =>
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
            className=Css.(style([marginTop(px(Theme.space(1)))]))
          />,
        |])
      | Error(error) =>
        switch (error) {
        | CouldNotPersistVenture =>
          "Your submission could not be persisted, probably due to network connectivity."
          |> message(Error)
        | MaxPartnersReached =>
          "The maximum number of partners we currently support in a venture has been reached"
          |> message(Error)
        | PartnerAlreadyProposed =>
          "This user has already been proposed to join" |> message(Error)
        | PartnerAlreadyExists =>
          "User is already a partner of this venture" |> message(Error)
        | CouldNotJoinVenture =>
          {|Error joining venture. Perhaps you have not been accepted yet or
        if this was your first time logging in to Misthos the Venture will become available after the inviting partner has logged in again.|}
          |> message(Error)
        | CouldNotLoadVenture => "Error loading venture" |> message(Error)
        }
      | Success(success) =>
        switch (success) {
        | ProcessStarted(_) =>
          "Your proposal has been submited" |> message(Success)
        | ProcessEndorsed(_) =>
          "Your endorsement has been submited" |> message(Success)
        | ProcessRejected(_) =>
          "Your rejection has been submited" |> message(Success)
        }
      },
  };
};
