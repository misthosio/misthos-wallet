type t =
  | Idle
  | Pending(WebWorker.correlationId)
  | Error(VentureWorkerMessage.cmdError)
  | Success(VentureWorkerMessage.cmdSuccess);

let component = ReasonReact.statelessComponent("CommandStatus");

let make = (~trackId, ~lastResponse, children) => {
  ...component,
  reducer: (_action: unit, _state) => ReasonReact.NoUpdate,
  render: (_) => {
    let initialCmdStatus =
      switch (trackId) {
      | Some(correlationId) => Pending(correlationId)
      | None => Idle
      };
    let cmdStatus =
      switch (initialCmdStatus, lastResponse) {
      | (Pending(correlationId), Some((responseId, response)))
          when correlationId == responseId =>
        switch ((response: VentureWorkerMessage.cmdResponse)) {
        | Ok(success) => Success(success)
        | Error(err) => Error(err)
        }
      | _ => initialCmdStatus
      };
    children(~cmdStatus);
  },
};
