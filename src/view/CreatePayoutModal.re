include ViewCommon;

[@bs.module] external remove : string = "../assets/img/remove.svg";

module View = ViewModel.CreatePayoutView;

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
  | EnterMax
  | AddAnother
  | ProposePayout;

let component = ReasonReact.reducerComponent("CreatePayout");

module Styles = {
  open Css;
  let maxButton =
    style([
      important(paddingLeft(px(4))),
      important(paddingRight(px(4))),
      color(rgba(0, 0, 0, 0.54)),
      unsafe("minWidth", "min-content"),
    ]);
  let maxWidth = style([maxWidth(`percent(99.0))]);
  let buttonPadding = style([paddingLeft(px(4))]);
};

let make =
    (
      ~viewData: View.t,
      ~commands: CommandExecutor.commands,
      ~cmdStatus: CommandExecutor.cmdStatus,
      _children,
    ) => {
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
          let max = viewData.max(address, state.destinations, defaultFee);
          let (inputAmount, btcAmount) =
            state.inputAmount |> BTC.gt(max) ?
              (max, max |> BTC.format) :
              (state.inputAmount, state.inputs.btcAmount);
          (
            viewData.summary(
              [(address, inputAmount), ...state.destinations],
              defaultFee,
            ),
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
          let max =
            viewData.max(
              state.inputDestination,
              state.destinations,
              defaultFee,
            );
          let (inputAmount, btcAmount) =
            inputAmount |> BTC.gt(max) ?
              (max, max |> BTC.format) : (inputAmount, btcAmount);
          (
            viewData.summary(
              [(state.inputDestination, inputAmount), ...state.destinations],
              defaultFee,
            ),
            inputAmount,
            btcAmount,
          );
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
      let destinations =
        if (state.inputDestination != ""
            && state.inputAmount
            |> BTC.gt(BTC.zero)) {
          [
            (state.inputDestination, state.inputAmount),
            ...state.destinations,
          ];
        } else {
          state.destinations;
        };
      commands.proposePayout(
        ~accountIdx=WalletTypes.AccountIndex.default,
        ~destinations,
        ~fee=defaultFee,
      );
      ReasonReact.NoUpdate;
    | AddAnother =>
      if (state.inputDestination != ""
          && state.inputAmount
          |> BTC.gt(BTC.zero)) {
        ReasonReact.Update({
          ...state,
          destinations: [
            (state.inputDestination, state.inputAmount),
            ...state.destinations,
          ],
          inputDestination: "",
          inputAmount: BTC.zero,
          inputs: {
            recipientAddress: "",
            btcAmount: "",
          },
        });
      } else {
        ReasonReact.NoUpdate;
      }
    | EnterMax =>
      let max =
        viewData.max(state.inputDestination, state.destinations, defaultFee);
      ReasonReact.SideEffects(
        (({send}) => send(ChangeBTCAmount(max |> BTC.format))),
      );
    },
  render: ({send, state: {viewData, inputs, destinations, summary}}) => {
    let feedback =
      switch (cmdStatus) {
      | Pending(_) => <Spinner text="waiting for result" />
      | Error(_) => "Could not execute teh command" |> text
      | _ => ReasonReact.null
      };
    let destinationList =
      ReasonReact.array(
        Array.of_list(
          destinations
          |> List.mapi((idx, (address, amount)) =>
               MaterialUi.(
                 <TableRow key=(idx |> string_of_int)>
                   <TableCell padding=`None>
                     <b> (address |> text) </b>
                   </TableCell>
                   <TableCell numeric=true padding=`None>
                     (BTC.format(amount) ++ " BTC" |> text)
                   </TableCell>
                   <TableCell numeric=true padding=`None>
                     <IconButton onClick=(_e => Js.log("TODO"))>
                       <img src=remove alt="Remove" />
                     </IconButton>
                   </TableCell>
                 </TableRow>
               )
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
            ensuring=true
            endAdornment=MaterialUi.(
                           <InputAdornment position=`End>
                             <MButton
                               className=Styles.maxButton
                               size=`Small
                               variant=Flat
                               onClick=(_e => send(EnterMax))>
                               (text("Max"))
                             </MButton>
                           </InputAdornment>
                         )
          />
          <MButton fullWidth=true onClick=(_e => send(AddAnother))>
            (text("Add to Summary"))
          </MButton>
        </div>
      body2=
        <div>
          <MTypography variant=`Title> (text("Summary")) </MTypography>
          MaterialUi.(
            <Table>
              <TableBody>
                destinationList
                <TableRow key="networkFee">
                  <TableCell className=Styles.maxWidth padding=`None>
                    <b> ("NETWORK FEE" |> text) </b>
                  </TableCell>
                  <TableCell numeric=true padding=`None>
                    (BTC.format(summary.networkFee) ++ " BTC" |> text)
                  </TableCell>
                  <TableCell />
                </TableRow>
                <TableRow key="misthosFee">
                  <TableCell padding=`None>
                    <b> ("MISTHOS FEE" |> text) </b>
                  </TableCell>
                  <TableCell numeric=true padding=`None>
                    (BTC.format(summary.misthosFee) ++ " BTC" |> text)
                  </TableCell>
                  <TableCell />
                </TableRow>
              </TableBody>
            </Table>
          )
          <MButton fullWidth=true onClick=(_e => send(ProposePayout))>
            (text("Propose Payout"))
          </MButton>
          feedback
        </div>
    />;
  },
};
