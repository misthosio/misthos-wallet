include ViewCommon;

open PrimitiveTypes;

open WalletTypes;

type action =
  | PreSubmit(string)
  | PreSubmitError(string)
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
  submitCustodianKeyChain: (~keyChain: CustodianKeyChain.public) => unit,
  proposePayout:
    (
      ~accountIdx: accountIdx,
      ~payoutTx: PayoutTransaction.t,
      ~signatures: array(option((string, string)))
    ) =>
    unit,
  endorsePayout:
    (~signatures: array(option((string, string))), ~processId: processId) =>
    unit,
  rejectPayout: (~processId: processId) => unit,
  preSubmit: string => unit,
  preSubmitError: string => unit,
};

type cmdStatus =
  | Idle
  | PreSubmit(string)
  | PreSubmitError(string)
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
    submitCustodianKeyChain: (~keyChain) =>
      send(CommandExecuted(commands.submitCustodianKeyChain(~keyChain))),
    proposePayout: (~accountIdx, ~payoutTx, ~signatures) =>
      send(
        CommandExecuted(
          commands.proposePayout(~accountIdx, ~payoutTx, ~signatures),
        ),
      ),
    endorsePayout: (~signatures, ~processId) =>
      send(
        CommandExecuted(commands.endorsePayout(~signatures, ~processId)),
      ),
    rejectPayout: (~processId) =>
      send(CommandExecuted(commands.rejectPayout(~processId))),
    preSubmit: message => send(PreSubmit(message)),
    preSubmitError: message => send(PreSubmitError(message)),
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
      | PreSubmit(message) =>
        ReasonReact.Update({cmdStatus: PreSubmit(message)})
      | PreSubmitError(message) =>
        ReasonReact.Update({cmdStatus: PreSubmitError(message)})
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
    | SubmitKeys
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
      | PreSubmit(message) =>
        ReasonReact.array([|
          <MTypography variant=`Body2> (message |> text) </MTypography>,
          <MaterialUi.LinearProgress
            className=Css.(style([marginTop(px(Theme.space(1)))]))
          />,
        |])
      | Pending(_) =>
        ReasonReact.array([|
          <MTypography variant=`Body2>
            (
              (
                switch (action) {
                | CreateVenture => "Venture is being created"
                | JoinVenture => "Joining venture"
                | LoadVenture => "Loading venture"
                | SubmitKeys => "Your public keys are being submitted"
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
      | PreSubmitError(error) => error |> message(Error)
      | Error(error) =>
        switch (error) {
        | CouldNotPersistVenture =>
          "Your submission could not be persisted, probably due to network connectivity."
          |> message(Error)
        | NotACustodian =>
          "You are not a custodian of this venture" |> message(Error)
        | UserIdDoesNotExist =>
          "Blockstack id does not exist, or is corrupted" |> message(Error)
        | MaxPartnersReached =>
          "The maximum number of partners we currently support in a venture has been reached"
          |> message(Error)
        | PartnerAlreadyProposed =>
          "This user has already been proposed to join" |> message(Error)
        | PartnerAlreadyExists =>
          "User is already a partner of this venture" |> message(Error)
        | CouldNotJoinVenture =>
          "Error joining venture. Please contact us if this problem persists."
          |> message(Error)
        | CouldNotLoadVenture =>
          "Error loading venture. Please contact us if this problem persist"
          |> message(Error)
        }
      | Success(success) =>
        switch (success) {
        | KeyChainSubmitted =>
          "Your public Keys have been submitted" |> message(Success)
        | ProcessStarted(_) =>
          "Your proposal has been submitted" |> message(Success)
        | ProcessEndorsed(_) =>
          "Your endorsement has been submitted" |> message(Success)
        | ProcessRejected(_) =>
          "Your rejection has been submitted" |> message(Success)
        }
      },
  };
};
