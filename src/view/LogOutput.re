include ViewCommon;

let component = ReasonReact.reducerComponent("LogOutput");

let make = (~ventureId, _children) => {
  ...component,
  didMount: ({send}) =>
    Js.Promise.(
      WorkerUtils.loadVenture(ventureId)
      |> then_(log => send(log) |> resolve)
    )
    |> ignore,
  initialState: () => None,
  reducer: (log: EventLog.t, _) => ReasonReact.Update(Some(log)),
  render: ({state}) =>
    <div>
      {
        switch (state) {
        | None => ReasonReact.null
        | Some(log) => log |> EventLog.encode |> Json.stringify |> text
        }
      }
    </div>,
};
