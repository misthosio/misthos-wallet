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
