include ViewCommon;

open WalletTypes;

module View = ViewModel.Payout;

type inputs = {
  recipientAddress: string,
  btcAmount: string,
};

type state = {
  viewData: View.t,
  inputs,
};

type action =
  | ChangeRecipientAddress(string)
  | ChangeBTCAmount(string);

let component = ReasonReact.reducerComponent("Payout");

let make =
    (~viewData: View.t, ~commands: VentureWorkerClient.Cmd.t, _children) => {
  ...component,
  initialState: () => {
    viewData,
    inputs: {
      recipientAddress: "",
      btcAmount: "",
    },
  },
  reducer: (action, state) =>
    switch (action) {
    | ChangeRecipientAddress(address) =>
      ReasonReact.Update({
        ...state,
        inputs: {
          ...state.inputs,
          recipientAddress: address,
        },
      })
    },
  render: ({send, state: {viewData, inputs}}) =>
    <div>
      <TitleBar titles=["Create A Payout"] />
      <MTypography variant=`Title>
        (viewData.ventureName |> text)
      </MTypography>
      <MTypography variant=`Display2>
        <b key="currentSpendable">
          (viewData.balance |> BTC.format |> text)
        </b>
        ("BTC" |> text)
      </MTypography>
      (text("Proposed"))
      <MInput
        placeholder="Recipient Address"
        value=(`String(inputs.recipientAddress))
        onChange=(e => send(ChangeRecipientAddress(extractString(e))))
        autoFocus=false
        fullWidth=true
      />
      <MInput
        placeholder="BTC amount"
        value=(`String(inputs.recipientAddress))
        onChange=(e => send(ChangeBTCAmount(extractString(e))))
        autoFocus=false
        fullWidth=true
      />
    </div>,
};
