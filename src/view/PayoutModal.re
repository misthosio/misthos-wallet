include ViewCommon;

module View = ViewModel.PayoutView;

let defaultFee = BTC.fromSatoshis(100L);

type inputs = {
  recipientAddress: string,
  btcAmount: string,
};

type state = {
  viewData: View.t,
  destinations: list((string, BTC.t)),
  inputDestination: string,
  inputAmount: BTC.t,
  summary: PayoutTransaction.summary,
  inputs,
};

type action =
  | ChangeRecipientAddress(string)
  | ChangeBTCAmount(string)
  | ProposePayout;

let component = ReasonReact.reducerComponent("Payout");

module Styles = {
  open Css;
  let balance = style([fontSize(vw(2.0))]);
};

let make =
    (~viewData: View.t, ~commands: VentureWorkerClient.Cmd.t, _children) => {
  ...component,
  initialState: () => {
    viewData,
    destinations: [],
    summary: viewData.initialSummary,
    inputDestination: "",
    inputAmount: BTC.zero,
    inputs: {
      recipientAddress: "",
      btcAmount: "",
    },
  },
  reducer: (action, {viewData} as state) =>
    switch (action) {
    | ChangeRecipientAddress(address) =>
      let (summary, inputDestination, inputAmount, btcAmount) =
        if (address |> viewData.isAddressValid) {
          let max = viewData.max(address, [], defaultFee);
          let (inputAmount, btcAmount) =
            state.inputAmount |> BTC.gt(max) ?
              (max, max |> BTC.format) :
              (state.inputAmount, state.inputs.btcAmount);
          (
            viewData.summary([(address, inputAmount)], defaultFee),
            address,
            inputAmount,
            btcAmount,
          );
        } else {
          (state.summary, "", state.inputAmount, state.inputs.btcAmount);
        };
      ReasonReact.Update({
        ...state,
        summary,
        inputDestination,
        inputAmount,
        inputs: {
          recipientAddress: address,
          btcAmount,
        },
      });
    | ChangeBTCAmount(amount) =>
      let (summary, inputAmount, btcAmount) = {
        let inputAmount = BTC.fromString(amount);
        let (inputAmount, btcAmount) =
          inputAmount |> BTC.isNaN ?
            (state.inputAmount, state.inputs.btcAmount) :
            (inputAmount, amount);
        if (state.inputDestination != "") {
          let max = viewData.max(state.inputDestination, [], defaultFee);
          let (inputAmount, btcAmount) =
            inputAmount |> BTC.gt(max) ?
              (max, max |> BTC.format) : (inputAmount, btcAmount);
          if (state.inputDestination != "") {
            (
              viewData.summary(
                [(state.inputDestination, inputAmount)],
                defaultFee,
              ),
              inputAmount,
              btcAmount,
            );
          } else {
            (state.summary, inputAmount, btcAmount);
          };
        } else {
          (state.summary, inputAmount, btcAmount);
        };
      };
      ReasonReact.Update({
        ...state,
        inputAmount,
        summary,
        inputs: {
          ...state.inputs,
          btcAmount,
        },
      });
    | ProposePayout =>
      commands.proposePayout(
        ~accountIdx=WalletTypes.AccountIndex.default,
        ~destinations=state.destinations,
        ~fee=defaultFee,
      );
      ReasonReact.NoUpdate;
    },
  render: ({send, state: {viewData, inputs, destinations, summary}}) => {
    let destinationList =
      ReasonReact.array(
        Array.of_list(
          destinations
          |> List.map(((address, amount)) =>
               <div> (text(address)) (text(BTC.format(amount))) </div>
             ),
        ),
      );
    <Body2
      titles=["Create A Payout"]
      body1=
        <div>
          <MTypography variant=`Title>
            (viewData.ventureName |> text)
          </MTypography>
          <Balance currentSpendable=viewData.balance />
          (text("Proposed recipients"))
          <ul>
            destinationList
            <li>
              (text("Network Fee - " ++ BTC.format(summary.networkFee)))
            </li>
            <li>
              (text("Misthos Fee - " ++ BTC.format(summary.misthosFee)))
            </li>
          </ul>
        </div>
      body2=
        <div>
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
        </div>
    />;
  },
};
