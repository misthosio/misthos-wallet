include ViewCommon;

open WalletTypes;

type action =
  | CommandExecuted(WebWorker.correlationId);

type commands = {
  proposePayout:
    (
      ~accountIdx: accountIdx,
      ~destinations: list((string, BTC.t)),
      ~fee: BTC.t
    ) =>
    unit,
};

type cmdStatus =
  | Idle
  | Pending(WebWorker.correlationId)
  | Response(VentureWorkerMessage.cmdResponse);

type state = {cmdStatus};

let component = ReasonReact.reducerComponent("CommandExecuter");

let make = (~commands: VentureWorkerClient.Cmd.t, ~lastResponse, children) => {
  let wrapCommands = send => {
    proposePayout: (~accountIdx, ~destinations, ~fee) =>
      send(
        CommandExecuted(
          commands.proposePayout(~accountIdx, ~destinations, ~fee),
        ),
      ),
  };
  {
    ...component,
    initialState: () => {cmdStatus: Idle},
    willReceiveProps: ({state: {cmdStatus}}) => {
      cmdStatus:
        switch (cmdStatus, lastResponse) {
        | (Pending(correlationId), Some((responseId, response)))
            when correlationId == responseId =>
          Response(response)
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