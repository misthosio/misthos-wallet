include ViewCommon;

module View = ViewModel.PayoutView;

type inputs = {
  recipientAddress: string,
  btcAmount: string,
};

type state = {
  viewData: View.t,
  destinations: list((string, BTC.t)),
  misthosFee: BTC.t,
  networkFee: BTC.t,
  inputs,
};

type action =
  | ChangeRecipientAddress(string)
  | ChangeBTCAmount(string)
  | ProposePayout;

let component = ReasonReact.reducerComponent("Payout");

let make =
    (~viewData: View.t, ~commands: VentureWorkerClient.Cmd.t, _children) => {
  ...component,
  initialState: () => {
    viewData,
    destinations: [],
    misthosFee: BTC.zero,
    networkFee: BTC.zero,
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
    | ChangeBTCAmount(amount) =>
      ReasonReact.Update({
        ...state,
        inputs: {
          ...state.inputs,
          btcAmount: amount,
        },
      })
    | ProposePayout =>
      commands.proposePayout(
        ~accountIdx=WalletTypes.AccountIndex.default,
        ~destinations=state.destinations,
        ~fee=BTC.fromSatoshis(100L),
      );
      ReasonReact.NoUpdate;
    },
  render:
    (
      {send, state: {viewData, inputs, destinations, networkFee, misthosFee}},
    ) => {
    let destinationList =
      ReasonReact.array(
        Array.of_list(
          destinations
          |> List.map(((address, amount)) =>
               <div> (text(address)) (text(BTC.format(amount))) </div>
             ),
        ),
      );
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
      (text("Proposed recipients"))
      <ul>
        destinationList
        <li> (text("Network Fee - " ++ BTC.format(networkFee))) </li>
        <li> (text("Misthos Fee - " ++ BTC.format(misthosFee))) </li>
      </ul>
      <MInput
        placeholder="Recipient Address"
        value=(`String(inputs.recipientAddress))
        onChange=(e => send(ChangeRecipientAddress(extractString(e))))
        autoFocus=false
        fullWidth=true
      />
      <MInput
        placeholder="BTC amount"
        value=(`String(inputs.btcAmount))
        onChange=(e => send(ChangeBTCAmount(extractString(e))))
        autoFocus=false
        fullWidth=true
      />
      <MButton fullWidth=true onClick=(_e => send(ProposePayout))>
        (text("Propose Payout"))
      </MButton>
    </div>;
  },
};
