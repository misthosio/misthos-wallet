open Belt;

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
  payoutTx: option(PayoutTransaction.t),
  txHexs: Map.String.t(string),
  inputs,
};

type action =
  | ChangeRecipientAddress(string)
  | ChangeBTCAmount(string)
  | RemoveDestination(int)
  | SetFee(BTC.t)
  | InputHexsCollected(Map.String.t(string))
  | EnterMax
  | AddToSummary
  | ProposePayout
  | Freeze
  | Reset;

let component = ReasonReact.reducerComponent("CreatePayout");

module Styles = {
  open Css;
  let maxButton = style([color(Colors.clickableGray)]);
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
  if (inputAmount |. BTC.gt(BTC.zero) && inputDestination != "") {
    let max = viewData.max(inputDestination, destinations, fee);
    let (inputAmount, btcAmount) =
      inputAmount |. BTC.gt(max) ?
        (max, max |> BTC.format) : (inputAmount, btcAmount);
    let payoutTx =
      viewData.createPayoutTx(
        [(inputDestination, inputAmount), ...destinations],
        fee,
      );
    let summary = viewData.summary(payoutTx);
    {
      ...state,
      viewData,
      inputAmount,
      inputDestination,
      addressValid,
      canSubmitProposal: summary.spentWithFees |. BTC.gt(summary.networkFee),
      summary,
      payoutTx: Some(payoutTx),
      inputs: {
        btcAmount,
        recipientAddress,
      },
    };
  } else {
    let payoutTx = viewData.createPayoutTx(destinations, fee);
    let summary = viewData.summary(payoutTx);
    {
      ...state,
      viewData,
      inputAmount,
      inputDestination,
      addressValid,
      canSubmitProposal: summary.spentWithFees |. BTC.gt(summary.networkFee),
      summary,
      payoutTx: Some(payoutTx),
      inputs: {
        btcAmount,
        recipientAddress,
      },
    };
  };
};

let updateInputTxs = (send, state) =>
  switch (state.payoutTx) {
  | Some(payoutTx) =>
    Js.Global.setTimeout(
      () =>
        Js.Promise.(
          state.viewData.collectInputHexs(state.txHexs, payoutTx)
          |> then_(((knownHexs, _inputs)) =>
               send(InputHexsCollected(knownHexs)) |> resolve
             )
          |> ignore
        ),
      0,
    )
    |> ignore
  | None => ()
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
    payoutTx: None,
    txHexs: Map.String.empty,
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
      |. BTC.gt(state.summary.spentWithFees)
      || state.inputAmount
      |. BTC.gt(BTC.zero),
    ) {
    | (Success(_) | Error(_), _, canInput)
    | (Idle, false, canInput) =>
      switch (action) {
      | SetFee(fee) => ReasonReact.Update({...state, fee})
      | RemoveDestination(removeIdx) =>
        let state =
          {
            ...state,
            destinations:
              state.destinations
              |. Belt.List.mapWithIndexU((. idx, destination) =>
                   idx == removeIdx ? None : Some(destination)
                 )
              |. Belt.List.keepMapU((. d) => d),
          }
          |> updateState;

        ReasonReact.UpdateWithSideEffects(
          state,
          (({send, state}) => updateInputTxs(send, state)),
        );
      | ChangeRecipientAddress(address) when canInput == true =>
        let state =
          {
            ...state,
            inputs: {
              ...state.inputs,
              recipientAddress: address |> Js.String.trim,
            },
          }
          |> updateState;
        ReasonReact.UpdateWithSideEffects(
          state,
          (({send, state}) => updateInputTxs(send, state)),
        );
      | ChangeBTCAmount(amount) when canInput == true =>
        let state =
          {
            ...state,
            inputs: {
              ...state.inputs,
              btcAmount: amount == "." ? "0." : amount,
            },
          }
          |> updateState;
        ReasonReact.UpdateWithSideEffects(
          state,
          (({send, state}) => updateInputTxs(send, state)),
        );
      | AddToSummary when canInput == true =>
        if (state.inputDestination != ""
            && state.inputAmount
            |. BTC.gt(BTC.zero)) {
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
      | InputHexsCollected(knownHexs) =>
        ReasonReact.Update({...state, txHexs: knownHexs})
      | _ => ReasonReact.NoUpdate
      }
    | _ =>
      switch (action) {
      | InputHexsCollected(knownHexs) =>
        ReasonReact.Update({...state, txHexs: knownHexs})
      | Freeze => ReasonReact.Update({...state, frozen: true})
      | Reset =>
        ReasonReact.UpdateWithSideEffects(
          {...state, frozen: false},
          (_ => commands.reset()),
        )
      | ProposePayout =>
        switch (state.payoutTx) {
        | Some(payoutTx) =>
          if (! viewData.requiresLedgerSig) {
            commands.proposePayout(
              ~accountIdx=WalletTypes.AccountIndex.default,
              ~payoutTx,
              ~signatures=[||],
            );
          } else {
            Js.Global.setTimeout(
              () =>
                Js.Promise.(
                  viewData.collectInputHexs(state.txHexs, payoutTx)
                  |> then_(((_, inputs)) =>
                       viewData.signPayoutTx(payoutTx, inputs)
                     )
                  |> then_(
                       fun
                       | Ledger.Signatures(signatures) =>
                         commands.proposePayout(
                           ~accountIdx=WalletTypes.AccountIndex.default,
                           ~payoutTx,
                           ~signatures,
                         )
                         |> resolve
                       | WrongDevice =>
                         commands.preSubmitError(
                           "The device does not have the correct seed for signing",
                         )
                         |> resolve
                       | Error(Message(message)) =>
                         commands.preSubmitError(message) |> resolve
                       | Error(Unknown) =>
                         commands.preSubmitError(
                           "An unknown error has occured",
                         )
                         |> resolve,
                     )
                  |> ignore
                ),
              1,
            )
            |> ignore;
            commands.preSubmit(
              "Please confirm this proposal on your ledger device",
            );
          };
          ();
        | None => ()
        };
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
        address != "" && amount |. BTC.gt(BTC.zero) ?
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
        List.toArray([
          destinationRow(
            ~withRemoveBtn=false,
            0,
            inputDestination,
            inputAmount,
          ),
          ...destinations
             |. List.mapWithIndex((idx, (address, amount)) =>
                  destinationRow(idx + 1, address, amount)
                ),
        ]),
      );
    <Grid
      ?warning
      title1=("Propose A Payout" |> text)
      area3={
        <div>
          <MTypography gutterBottom=true variant=`Title>
            ("ADD A RECIPIENT" |> text)
          </MTypography>
          <MTypography variant=`Body2>
            (
              "AVAILABLE BALANCE: "
              ++ (viewData.balance.currentSpendable |> BTC.format)
              ++ " BTC"
              |> text
            )
          </MTypography>
          (
            if (viewData.allowCreation == true) {
              let error = addressValid ? None : Some("Address is BAD");
              <div>
                <MInput
                  placeholder="Recipient Address"
                  value=(`String(inputs.recipientAddress))
                  onChange=(
                    e => send(ChangeRecipientAddress(extractString(e)))
                  )
                  autoFocus=false
                  fullWidth=true
                  ?error
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
                <MButton
                  size=`Small
                  variant=Flat
                  fullWidth=true
                  onClick=(_e => send(AddToSummary))>
                  (text("+ Add Another Recipient"))
                </MButton>
              </div>;
            } else {
              ReasonReact.null;
            }
          )
        </div>
      }
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
      area5={<MTypography variant=`Body1> PolicyText.payout </MTypography>}
    />;
  },
};
