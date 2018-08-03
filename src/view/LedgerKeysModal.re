include ViewCommon;

module ViewData = ViewModel.LedgerKeysView;

type error =
  | LedgerError(LedgerJS.error)
  | WrongHardwareId;
type action =
  | SubmitPubKeys
  | FailedGettingKeys(error)
  | Completed(Bitcoin.HDNode.t);

type status =
  | Idle
  | InProgress
  | Completed;
type state = {
  status,
  error: option(error),
};

let component = ReasonReact.reducerComponent("LedgerKeys");
let make = (~viewData: ViewData.t, ~submitKeyChain, ~cmdStatus, _children) => {
  ...component,
  initialState: () => {status: Idle, error: None},
  reducer: (action, state: state) =>
    switch (action, state.status) {
    | (SubmitPubKeys, Idle) =>
      ReasonReact.UpdateWithSideEffects(
        {status: InProgress, error: None},
        (
          ({send}) =>
            Js.Promise.(
              viewData.getCustodianKeyChain()
              |> then_(
                   fun
                   | Ledger.Ok(keyChain) =>
                     switch (viewData.ledgerId) {
                     | Some(ledgerId)
                         when
                           keyChain
                           |> CustodianKeyChain.hardwareId
                           |> Js.Option.getExn != ledgerId =>
                       send(FailedGettingKeys(WrongHardwareId)) |> resolve
                     | _ => submitKeyChain(~keyChain) |> resolve
                     }
                   | Ledger.Error(error) =>
                     send(FailedGettingKeys(LedgerError(error))) |> resolve,
                 )
            )
            |> ignore
        ),
      )
    | (FailedGettingKeys(error), _) =>
      ReasonReact.Update({status: Idle, error: Some(error)})
    | _ => ReasonReact.NoUpdate
    },
  render: ({state, send}) => {
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
    let spinner =
      switch (state.status) {
      | InProgress => <Spinner text="InProgress" />
      | _ => ReasonReact.null
      };
    <Grid
      title1=("Connect Ledger" |> text)
      area3={
        <div>
          <p> (ledgerConnected |> text) </p>
          <p> (ledgerUpToDate |> text) </p>
          error
          spinner
          <MButton onClick=(ignoreEvent(() => send(SubmitPubKeys)))>
            ("Submit keys" |> text)
          </MButton>
        </div>
      }
    />;
  },
};
