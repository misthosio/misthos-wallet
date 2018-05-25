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
  addressValid: bool,
  summary: PayoutTransaction.summary,
  inputs,
};

type action =
  | ChangeRecipientAddress(string)
  | ChangeBTCAmount(string)
  | RemoveDestination(int)
  | EnterMax
  | AddToSummary
  | ProposePayout;

let component = ReasonReact.reducerComponent("CreatePayout");

module Styles = {
  open Css;
  let maxButton = style([color(rgba(0, 0, 0, 0.54))]);
  let maxWidth = style([maxWidth(`percent(99.0))]);
  let buttonPadding = style([paddingLeft(px(4))]);
  let noBorder = style([borderColor(`transparent)]);
};

let updateState =
    (
      {
        viewData,
        inputAmount,
        destinations,
        inputs: {btcAmount, recipientAddress},
      } as state,
    ) => {
  let (recipientAddress, inputDestination, addressValid) =
    if (recipientAddress |> viewData.isAddressValid) {
      (recipientAddress, recipientAddress, true);
    } else {
      (recipientAddress, "", false);
    };
  let newInputAmount = BTC.fromString(btcAmount);
  let (btcAmount, inputAmount) =
    if (btcAmount == "") {
      ("", BTC.zero);
    } else if (newInputAmount |> BTC.isNaN) {
      (inputAmount |> BTC.format, inputAmount);
    } else {
      (btcAmount, newInputAmount);
    };
  if (inputAmount |> BTC.gt(BTC.zero) && inputDestination != "") {
    let max = viewData.max(inputDestination, destinations, defaultFee);
    let (inputAmount, btcAmount) =
      inputAmount |> BTC.gt(max) ?
        (max, max |> BTC.format) : (inputAmount, btcAmount);
    {
      ...state,
      viewData,
      inputAmount,
      inputDestination,
      addressValid,
      summary:
        viewData.summary(
          [(inputDestination, inputAmount), ...destinations],
          defaultFee,
        ),
      inputs: {
        btcAmount,
        recipientAddress,
      },
    };
  } else {
    {
      ...state,
      viewData,
      inputAmount,
      inputDestination,
      addressValid,
      summary: viewData.summary(destinations, defaultFee),
      inputs: {
        btcAmount,
        recipientAddress,
      },
    };
  };
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
    addressValid: true,
    summary: viewData.initialSummary,
    inputDestination: "",
    inputAmount: BTC.zero,
    inputs: {
      recipientAddress: "",
      btcAmount: "",
    },
  },
  willReceiveProps: ({state}) => {...state, viewData},
  reducer: (action, {viewData} as state) =>
    switch (action) {
    | RemoveDestination(removeIdx) =>
      ReasonReact.Update(
        {
          ...state,
          destinations:
            state.destinations
            |. Belt.List.mapWithIndexU((. idx, destination) =>
                 idx == removeIdx ? None : Some(destination)
               )
            |. Belt.List.keepMapU((. d) => d),
        }
        |> updateState,
      )
    | ChangeRecipientAddress(address) =>
      ReasonReact.Update(
        {
          ...state,
          inputs: {
            ...state.inputs,
            recipientAddress: address,
          },
        }
        |> updateState,
      )
    | ChangeBTCAmount(amount) =>
      ReasonReact.Update(
        {
          ...state,
          inputs: {
            ...state.inputs,
            btcAmount: amount,
          },
        }
        |> updateState,
      )
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
    | AddToSummary =>
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
  render:
    ({send, state: {viewData, addressValid, inputs, destinations, summary}}) => {
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
                   <TableCell className=Styles.noBorder padding=`None>
                     <b> (address |> text) </b>
                   </TableCell>
                   <TableCell
                     numeric=true className=Styles.noBorder padding=`None>
                     (BTC.format(amount) ++ " BTC" |> text)
                   </TableCell>
                   <TableCell
                     numeric=true className=Styles.noBorder padding=`None>
                     <IconButton
                       onClick=(_e => send(RemoveDestination(idx)))>
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
      body1=(
              if (viewData.allowCreation == false) {
                <div>
                  <MTypography variant=`Title>
                    (viewData.ventureName |> text)
                  </MTypography>
                  <Balance
                    currentSpendable=viewData.balance.currentSpendable
                    reserved=viewData.balance.reserved
                  />
                </div>;
              } else {
                <div>
                  <MTypography variant=`Title>
                    (viewData.ventureName |> text)
                  </MTypography>
                  <Balance
                    currentSpendable=viewData.balance.currentSpendable
                    reserved=viewData.balance.reserved
                  />
                  {
                    let error = addressValid ? None : Some("Address is BAD");
                    <MInput
                      placeholder="Recipient Address"
                      value=(`String(inputs.recipientAddress))
                      onChange=(
                        e => send(ChangeRecipientAddress(extractString(e)))
                      )
                      autoFocus=false
                      fullWidth=true
                      ?error
                    />;
                  }
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
                  <MButton fullWidth=true onClick=(_e => send(AddToSummary))>
                    (text("Add to Summary"))
                  </MButton>
                </div>;
              }
            )
      body2=(
              if (viewData.allowCreation == false) {
                <div>
                  <MTypography variant=`Body2>
                    ("Cannot create Payout without unreserved balance" |> text)
                  </MTypography>
                </div>;
              } else {
                <div>
                  <MTypography variant=`Title>
                    (text("Summary"))
                  </MTypography>
                  MaterialUi.(
                    <Table>
                      <TableBody>
                        destinationList
                        <TableRow key="networkFee">
                          <TableCell
                            className=(
                              Styles.maxWidth ++ " " ++ Styles.noBorder
                            )
                            padding=`None>
                            <b> ("NETWORK FEE" |> text) </b>
                          </TableCell>
                          <TableCell
                            numeric=true
                            className=Styles.noBorder
                            padding=`None>
                            (BTC.format(summary.networkFee) ++ " BTC" |> text)
                          </TableCell>
                          <TableCell className=Styles.noBorder />
                        </TableRow>
                        <TableRow key="misthosFee">
                          <TableCell className=Styles.noBorder padding=`None>
                            <b> ("MISTHOS FEE" |> text) </b>
                          </TableCell>
                          <TableCell
                            numeric=true
                            className=Styles.noBorder
                            padding=`None>
                            (BTC.format(summary.misthosFee) ++ " BTC" |> text)
                          </TableCell>
                          <TableCell className=Styles.noBorder />
                        </TableRow>
                      </TableBody>
                    </Table>
                  )
                  <MButton fullWidth=true onClick=(_e => send(ProposePayout))>
                    (text("Propose Payout"))
                  </MButton>
                  feedback
                </div>;
              }
            )
    />;
  },
};
