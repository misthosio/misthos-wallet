include ViewCommon;

module ViewData = ViewModel.LedgerKeysView;

let component = ReasonReact.statelessComponent("VentureSettings");
let make =
    (
      ~viewData: ViewData.t,
      ~commands: CommandExecutor.commands,
      ~cmdStatus,
      _children,
    ) => {
  let executeSubmit = () => {
    commands.preSubmit("Please connect your ledger device");
    Js.Promise.(
      viewData.getCustodianKeyChain()
      |> then_(
           fun
           | Ledger.Ok(keyChain) =>
             commands.submitCustodianKeyChain(~keyChain) |> resolve
           | WrongDevice =>
             commands.preSubmitError("This device has the wrong seed")
             |> resolve
           | Ledger.Error(Message(message)) =>
             commands.preSubmitError(message) |> resolve
           | Error(Unknown) =>
             commands.preSubmitError("An unknown error has occured")
             |> resolve,
         )
    )
    |> ignore;
  };
  {
    ...component,
    render: _ => {
      let (ledgerIntegrater, keyStatus) =
        switch (viewData.ledgerId, viewData.ledgerUpToDate) {
        | (Some(_), true) => (
            "You have integrated your ledger device.",
            "Up to date",
          )
        | (Some(_), false) => (
            "You have integrated your ledger device.",
            "Needs rotating.",
          )
        | _ => (
            "You currently have no ledger device integrated into this venture.",
            "Not submitted",
          )
        };
      <Grid
        title1=("Venture Settings" |> text)
        area3={
          <div>
            <MTypography variant=`Title>
              ("Hardware Wallet Settings" |> text)
            </MTypography>
            <MTypography variant=`Body2 gutterBottom=true>
              (ledgerIntegrater |> text)
            </MTypography>
            <MTypography variant=`Body2>
              ("Key status: " ++ keyStatus |> text)
            </MTypography>
            (
              viewData.ledgerUpToDate && viewData.ledgerId |> Js.Option.isSome ?
                ReasonReact.null :
                <ProposeButton
                  onSubmit=executeSubmit
                  canSubmitProposal=true
                  withConfirmation=false
                  proposeText="Submit public keys"
                  cmdStatus
                />
            )
          </div>
        }
      />;
    },
  };
};
