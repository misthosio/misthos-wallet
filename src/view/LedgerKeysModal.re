include ViewCommon;

module ViewData = ViewModel.LedgerKeysView;

type error =
  | LedgerError(LedgerJS.error)
  | WrongHardwareId;
type action =
  | SubmitPubKeys
  | FailedGettingKeys(error);

type status =
  | Idle
  | InProgress
  | Completed;
type state = {
  viewData: ViewData.t,
  status,
  error: option(error),
};

let component = ReasonReact.reducerComponent("LedgerKeys");
let make = (~viewData: ViewData.t, ~submitKeyChain, ~cmdStatus, _children) => {
  ...component,
  initialState: () => {status: Idle, error: None, viewData},
  willReceiveProps: ({state}) => {...state, viewData},
  reducer: (action, state: state) =>
    switch (action, state.status) {
    | (SubmitPubKeys, Idle) =>
      ReasonReact.UpdateWithSideEffects(
        {...state, status: InProgress, error: None},
        (
          ({send}) =>
            Js.Promise.(
              viewData.getCustodianKeyChain()
              |> then_(
                   fun
                   | Ledger.Ok(keyChain) =>
                     submitKeyChain(~keyChain) |> resolve
                   | WrongDevice =>
                     send(FailedGettingKeys(WrongHardwareId)) |> resolve
                   | Ledger.Error(error) =>
                     send(FailedGettingKeys(LedgerError(error))) |> resolve,
                 )
            )
            |> ignore
        ),
      )
    | (FailedGettingKeys(error), _) =>
      ReasonReact.Update({...state, status: Idle, error: Some(error)})
    | _ => ReasonReact.NoUpdate
    },
  render: ({state, send}) => {
    let viewData = state.viewData;
    let ledgerConnected =
      switch (viewData.ledgerId) {
      | Some(_) => "Ledger is connected"
      | None => "Ledger is not connected"
      };
    let ledgerUpToDate =
      viewData.ledgerUpToDate ?
        "Ledger pub keys are up to date" : "Ledger keys need rotating";
    let error =
      switch (state.error) {
      | Some(LedgerError(error)) => LedgerJS.errorToString(error) |> text
      | Some(WrongHardwareId) => "This ledger has the wrong seed" |> text
      | None => ReasonReact.null
      };
    <Grid
      title1=("Connect Ledger" |> text)
      area3={
        <div>
          <p> (ledgerConnected |> text) </p>
          <p> (ledgerUpToDate |> text) </p>
          error
          (
            viewData.ledgerUpToDate && viewData.ledgerId |> Js.Option.isSome ?
              ReasonReact.null :
              ReasonReact.array([|
                <MButton onClick=(ignoreEvent(() => send(SubmitPubKeys)))>
                  ("Submit keys" |> text)
                </MButton>,
                <CommandExecutor.Status cmdStatus action=SubmitKeys />,
              |])
          )
        </div>
      }
    />;
  },
};
