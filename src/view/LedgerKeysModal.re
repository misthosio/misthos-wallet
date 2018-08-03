include ViewCommon;

module ViewData = ViewModel.LedgerKeysView;

type action =
  | SubmitPubKeys
  | FailedGettingKeys(LedgerJS.error)
  | Completed(Bitcoin.HDNode.t);

type status =
  | Idle
  | InProgress
  | Completed;
type state = {status};

let component = ReasonReact.reducerComponent("LedgerKeys");
let make = (~viewData: ViewData.t, _children) => {
  ...component,
  initialState: () => {status: Idle},
  reducer: (action, _state: state) =>
    switch (action) {
    | SubmitPubKeys =>
      ReasonReact.UpdateWithSideEffects(
        {status: InProgress},
        (
          ({send}) =>
            Js.Promise.(
              viewData.getCustodianKeyChain()
              |> then_(
                   fun
                   | Ledger.Ok(keyChain) =>
                     Js.log2(
                       "key chain:",
                       keyChain
                       |> CustodianKeyChain.hdNode
                       |> Bitcoin.HDNode.toBase58,
                     )
                     |> resolve
                   | Ledger.Error(error) => Js.log(error) |> resolve,
                 )
            )
            |> ignore
        ),
      )
    },
  render: ({state, send}) =>
    <Grid
      title1=("Connect Ledger" |> text)
      area3={
        <div>
          <MButton onClick=(ignoreEvent(() => send(SubmitPubKeys)))>
            ("Submit keys" |> text)
          </MButton>
        </div>
      }
    />,
};
