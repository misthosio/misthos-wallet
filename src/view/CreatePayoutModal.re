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
  canSubmitProposal: bool,
  frozen: bool,
  summary: PayoutTransaction.summary,
  inputs,
};

type action =
  | ChangeRecipientAddress(string)
  | ChangeBTCAmount(string)
  | RemoveDestination(int)
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
  let buttonPadding = style([paddingLeft(px(4))]);
  let noBorder = style([borderColor(`transparent), whiteSpace(`nowrap)]);
  let spaceBetween = align =>
    style([
      display(`flex),
      justifyContent(`spaceBetween),
      alignItems(align),
    ]);
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
    let summary =
      viewData.summary(
        [(inputDestination, inputAmount), ...destinations],
        defaultFee,
      );
    {
      ...state,
      viewData,
      inputAmount,
      inputDestination,
      addressValid,
      canSubmitProposal: summary.misthosFee |> BTC.gt(BTC.zero),
      summary,
      inputs: {
        btcAmount,
        recipientAddress,
      },
    };
  } else {
    let summary = viewData.summary(destinations, defaultFee);
    {
      ...state,
      viewData,
      inputAmount,
      inputDestination,
      addressValid,
      canSubmitProposal: summary.misthosFee |> BTC.gt(BTC.zero),
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
  reducer: (action, {viewData} as state) =>
    switch (cmdStatus, state.frozen) {
    | (Success(_) | Error(_), _)
    | (Idle, false) =>
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
          viewData.max(
            state.inputDestination,
            state.destinations,
            defaultFee,
          );
        ReasonReact.SideEffects(
          (({send}) => send(ChangeBTCAmount(max |> BTC.format))),
        );
      | Freeze => ReasonReact.Update({...state, frozen: true})
      | Reset =>
        ReasonReact.UpdateWithSideEffects(
          {...state, frozen: false},
          ((_) => commands.reset()),
        )
      | ProposePayout => ReasonReact.NoUpdate
      }
    | _ =>
      switch (action) {
      | Freeze => ReasonReact.Update({...state, frozen: true})
      | Reset =>
        ReasonReact.UpdateWithSideEffects(
          {...state, frozen: false},
          ((_) => commands.reset()),
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
          inputs,
          destinations,
          summary,
        },
      },
    ) => {
    let destinationList =
      ReasonReact.array(
        Array.of_list(
          destinations
          |> List.mapi((idx, (address, amount)) =>
               MaterialUi.(
                 <TableRow key=(idx |> string_of_int)>
                   <TableCell
                     className=(
                       Styles.spaceBetween(`center) ++ " " ++ Styles.noBorder
                     )
                     padding=`None>
                     <b> (address |> text) </b>
                     <IconButton
                       onClick=(_e => send(RemoveDestination(idx)))>
                       <img src=remove alt="Remove" />
                     </IconButton>
                   </TableCell>
                   <TableCell
                     numeric=true className=Styles.noBorder padding=`None>
                     (BTC.format(amount) ++ " BTC" |> text)
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
                    (text("Add another Recipient"))
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
                  <ScrollList>
                    MaterialUi.(
                      <Table>
                        <TableBody>
                          destinationList
                          <TableRow key="networkFee">
                            <TableCell className=Styles.noBorder padding=`None>
                              <b> ("NETWORK FEE" |> text) </b>
                            </TableCell>
                            <TableCell
                              numeric=true
                              className=(
                                Styles.maxWidth ++ " " ++ Styles.noBorder
                              )
                              padding=`None>
                              (
                                BTC.format(summary.networkFee)
                                ++ " BTC"
                                |> text
                              )
                            </TableCell>
                          </TableRow>
                          <TableRow key="misthosFee">
                            <TableCell className=Styles.noBorder padding=`None>
                              <b> ("MISTHOS FEE" |> text) </b>
                            </TableCell>
                            <TableCell
                              numeric=true
                              className=Styles.noBorder
                              padding=`None>
                              (
                                BTC.format(summary.misthosFee)
                                ++ " BTC"
                                |> text
                              )
                            </TableCell>
                          </TableRow>
                        </TableBody>
                      </Table>
                    )
                  </ScrollList>
                  <div className=(Styles.spaceBetween(`baseline))>
                    <MaterialUi.Typography variant=`Body2>
                      ("TOTAL PAYOUT" |> text)
                    </MaterialUi.Typography>
                    <MTypography variant=`Subheading>
                      (BTC.format(summary.spentWithFees) ++ " BTC" |> text)
                    </MTypography>
                  </div>
                  <ProposeButton
                    onPropose=(() => send(Freeze))
                    onSubmit=(() => send(ProposePayout))
                    onCancel=(() => send(Reset))
                    canSubmitProposal
                    proposeText="Propose payout"
                    cmdStatus
                  />
                </div>;
              }
            )
    />;
  },
};
