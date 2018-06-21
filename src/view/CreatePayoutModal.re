include ViewCommon;

module View = ViewModel.CreatePayoutView;

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
  canSubmitProposal: bool,
  frozen: bool,
  fee: BTC.t,
  summary: PayoutTransaction.summary,
  inputs,
};

type action =
  | ChangeRecipientAddress(string)
  | ChangeBTCAmount(string)
  | RemoveDestination(int)
  | SetFee(BTC.t)
  | EnterMax
  | AddToSummary
  | ProposePayout
  | Freeze
  | Reset;

let component = ReasonReact.reducerComponent("CreatePayout");

module Styles = {
  open Css;
  let maxButton = style([color(rgba(0, 0, 0, 0.54))]);
  let maxWidth = style([width(`percent(99.0))]);
  let cellHeight = style([height(px(49))]);
  let buttonPadding = style([paddingLeft(px(4))]);
  let noBorder = style([borderColor(`transparent), whiteSpace(`nowrap)]);
  let spaceBetween = align =>
    style([
      display(`flex),
      justifyContent(`spaceBetween),
      alignItems(align),
    ]);
  let total =
    style([
      backgroundColor(Colors.white),
      position(sticky),
      bottom(px(0)),
    ]);
};

let updateState =
    (
      {
        viewData,
        inputAmount,
        destinations,
        inputs: {btcAmount, recipientAddress},
        fee,
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
    let max = viewData.max(inputDestination, destinations, fee);
    let (inputAmount, btcAmount) =
      inputAmount |> BTC.gt(max) ?
        (max, max |> BTC.format) : (inputAmount, btcAmount);
    let summary =
      viewData.summary(
        [(inputDestination, inputAmount), ...destinations],
        fee,
      );
    {
      ...state,
      viewData,
      inputAmount,
      inputDestination,
      addressValid,
      canSubmitProposal: summary.spentWithFees |> BTC.gt(summary.networkFee),
      summary,
      inputs: {
        btcAmount,
        recipientAddress,
      },
    };
  } else {
    let summary = viewData.summary(destinations, fee);
    {
      ...state,
      viewData,
      inputAmount,
      inputDestination,
      addressValid,
      canSubmitProposal: summary.spentWithFees |> BTC.gt(summary.networkFee),
      summary,
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
    frozen: false,
    fee: BTC.zero,
    viewData,
    canSubmitProposal: false,
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
  didMount: ({send}) =>
    Js.Promise.(
      BitcoinFeesClient.fetchFees()
      |> then_((fees: BitcoinFeesClient.recomendedFees) =>
           send(SetFee(fees.hourFee)) |> resolve
         )
    )
    |> ignore,
  reducer: (action, {viewData} as state) =>
    switch (
      cmdStatus,
      state.frozen,
      viewData.balance.currentSpendable
      |> BTC.gt(state.summary.spentWithFees)
      || state.inputAmount
      |> BTC.gt(BTC.zero),
    ) {
    | (Success(_) | Error(_), _, canInput)
    | (Idle, false, canInput) =>
      switch (action) {
      | SetFee(fee) => ReasonReact.Update({...state, fee})
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
      | ChangeRecipientAddress(address) when canInput == true =>
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
      | ChangeBTCAmount(amount) when canInput == true =>
        ReasonReact.Update(
          {
            ...state,
            inputs: {
              ...state.inputs,
              btcAmount: amount == "." ? "0." : amount,
            },
          }
          |> updateState,
        )
      | AddToSummary when canInput == true =>
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
      | EnterMax when canInput == true =>
        let max =
          viewData.max(state.inputDestination, state.destinations, state.fee);
        ReasonReact.SideEffects(
          (({send}) => send(ChangeBTCAmount(max |> BTC.format))),
        );
      | Freeze => ReasonReact.Update({...state, frozen: true})
      | Reset =>
        ReasonReact.UpdateWithSideEffects(
          {...state, frozen: false},
          (_ => commands.reset()),
        )
      | _ => ReasonReact.NoUpdate
      }
    | _ =>
      switch (action) {
      | Freeze => ReasonReact.Update({...state, frozen: true})
      | Reset =>
        ReasonReact.UpdateWithSideEffects(
          {...state, frozen: false},
          (_ => commands.reset()),
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
          ~fee=state.fee,
        );
        ReasonReact.NoUpdate;
      | _ => ReasonReact.NoUpdate
      }
    },
  render:
    (
      {
        send,
        state: {
          canSubmitProposal,
          viewData,
          addressValid,
          inputDestination,
          inputAmount,
          inputs,
          destinations,
          summary,
        },
      },
    ) => {
    let warning =
      switch (Environment.get().network) {
      | Testnet => Some(WarningsText.testnet)
      | _ => None
      };

    let destinationRow = (~withRemoveBtn=true, idx, address, amount) =>
      MaterialUi.(
        address != "" && amount |> BTC.gt(BTC.zero) ?
          <TableRow key=(idx |> string_of_int)>
            <TableCell
              className=(
                Styles.spaceBetween(`center)
                ++ " "
                ++ Styles.noBorder
                ++ " "
                ++ Styles.cellHeight
              )
              padding=`None>
              <MTypography variant=`Body2> (address |> text) </MTypography>
              (
                withRemoveBtn ?
                  <IconButton
                    onClick=(_e => send(RemoveDestination(idx - 1)))>
                    Icons.remove
                  </IconButton> :
                  ReasonReact.null
              )
            </TableCell>
            <TableCell numeric=true className=Styles.noBorder padding=`None>
              <MTypography variant=`Body2>
                (BTC.format(amount) ++ " BTC" |> text)
              </MTypography>
            </TableCell>
          </TableRow> :
          ReasonReact.null
      );
    let destinationList =
      ReasonReact.array(
        Array.of_list([
          destinationRow(
            ~withRemoveBtn=false,
            0,
            inputDestination,
            inputAmount,
          ),
          ...destinations
             |> List.mapi((idx, (address, amount)) =>
                  destinationRow(idx + 1, address, amount)
                ),
        ]),
      );
    <Grid
      ?warning
      title1=("Propose A Payout" |> text)
      area3=(
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
                <div className=ScrollList.containerStyles>
                  <MTypography variant=`Title>
                    (viewData.ventureName |> text)
                  </MTypography>
                  <ScrollList>
                    <Balance
                      currentSpendable=viewData.balance.currentSpendable
                      reserved=viewData.balance.reserved
                    />
                    <MTypography variant=`Title>
                      (text("Enter Recipient Details"))
                    </MTypography>
                    {
                      let error = addressValid ? None : Some("Address is BAD");
                      <MInput
                        placeholder="Recipient Address"
                        value=(`String(inputs.recipientAddress))
                        onChange=(
                          e =>
                            send(ChangeRecipientAddress(extractString(e)))
                        )
                        autoFocus=false
                        fullWidth=true
                        ?error
                      />;
                    }
                    <MInput
                      placeholder="BTC amount"
                      value=(`String(inputs.btcAmount))
                      onChange=(
                        e => send(ChangeBTCAmount(extractString(e)))
                      )
                      autoFocus=false
                      fullWidth=true
                      ensuring=true
                      endAdornment=MaterialUi.(
                                     <InputAdornment position=`End>
                                       <MButton
                                         gutterTop=false
                                         className=Styles.maxButton
                                         size=`Small
                                         variant=Flat
                                         onClick=(_e => send(EnterMax))>
                                         (text("Max"))
                                       </MButton>
                                     </InputAdornment>
                                   )
                    />
                  </ScrollList>
                  <MButton
                    size=`Small
                    variant=Flat
                    fullWidth=true
                    onClick=(_e => send(AddToSummary))>
                    (text("Add Another Recipient"))
                  </MButton>
                </div>;
              }
            )
      area4=(
              if (viewData.allowCreation == false) {
                <div>
                  <MTypography variant=`Body2>
                    (
                      "You cannot create a Payout without an unreserved balance."
                      |> text
                    )
                  </MTypography>
                </div>;
              } else {
                <div className=ScrollList.containerStyles>
                  <MTypography variant=`Title>
                    (text("Summary"))
                  </MTypography>
                  <ScrollList>
                    MaterialUi.(
                      <Table>
                        <TableBody>
                          destinationList
                          <TableRow key="networkFee">
                            <TableCell className=Styles.noBorder padding=`None>
                              <MTypography variant=`Body2>
                                ("NETWORK FEE" |> text)
                              </MTypography>
                            </TableCell>
                            <TableCell
                              numeric=true
                              className=(
                                Styles.maxWidth ++ " " ++ Styles.noBorder
                              )
                              padding=`None>
                              <MTypography variant=`Body2>
                                (
                                  BTC.format(summary.networkFee)
                                  ++ " BTC"
                                  |> text
                                )
                              </MTypography>
                            </TableCell>
                          </TableRow>
                          <TableRow key="misthosFee">
                            <TableCell className=Styles.noBorder padding=`None>
                              <MTypography variant=`Body2>
                                ("MISTHOS FEE" |> text)
                              </MTypography>
                            </TableCell>
                            <TableCell
                              numeric=true
                              className=Styles.noBorder
                              padding=`None>
                              <MTypography variant=`Body2>
                                (
                                  BTC.format(summary.misthosFee)
                                  ++ " BTC"
                                  |> text
                                )
                              </MTypography>
                            </TableCell>
                          </TableRow>
                        </TableBody>
                      </Table>
                    )
                    <div
                      className=(
                        Styles.spaceBetween(`baseline) ++ " " ++ Styles.total
                      )>
                      <MaterialUi.Typography variant=`Body2>
                        ("TOTAL PAYOUT" |> text)
                      </MaterialUi.Typography>
                      <MTypography variant=`Subheading>
                        (BTC.format(summary.spentWithFees) ++ " BTC" |> text)
                      </MTypography>
                    </div>
                  </ScrollList>
                  <ProposeButton
                    onPropose=(() => send(Freeze))
                    onSubmit=(() => send(ProposePayout))
                    onCancel=(() => send(Reset))
                    canSubmitProposal
                    proposeText="Propose Payout"
                    cmdStatus
                  />
                </div>;
              }
            )
    />;
  },
};
