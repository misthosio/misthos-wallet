open Belt;

include ViewCommon;

module ViewData = ViewModel.VentureSettingsView;

module Styles = {
  open Css;
  let atRiskKeyStatus = style([color(Colors.error)]);
};

let policyDescription =
  fun
  | Policy.Unanimous => "Unanimous"
  | Policy.UnanimousMinusOne => "Unanimous minus 1"
  | Policy.Percentage({percentage}) => string_of_int(percentage) ++ "%"
  | Policy.AtLeast({n}) => "At least " ++ string_of_int(n);

let component = ReasonReact.statelessComponent("VentureSettings");
let make =
    (
      ~viewData: ViewData.t,
      ~commands: CommandExecutor.commands,
      ~cmdStatus,
      _children,
    ) => {
  let executeSubmit = () => {
    commands.preSubmit(
      "Please connect your Ledger device and open the BTC app",
    );
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
            "You have integrated your Ledger device.",
            "Up to date",
          )
        | (Some(_), false) => (
            "You have integrated your Ledger device.",
            "Needs rotating",
          )
        | _ => (
            "You currently have no Ledger device integrated into this venture.",
            "Not submitted",
          )
        };
      let nSigs =
        viewData.accountSettings.coSignerList
        |. Array.mapWithIndexU((. idx, nCoSigners) =>
             MaterialUi.(
               <TableRow>
                 <TableCell>
                   <MTypography variant=`Body2>
                     (string_of_int(idx) |> text)
                   </MTypography>
                 </TableCell>
                 <TableCell>
                   <MTypography variant=`Body2>
                     (
                       string_of_int(nCoSigners)
                       ++ "-of-"
                       ++ string_of_int(idx)
                       |> text
                     )
                   </MTypography>
                 </TableCell>
               </TableRow>
             )
           )
        |. Array.slice(~offset=1, ~len=10)
        |> ReasonReact.array;
      let needsKeyRotation =
        ! viewData.ledgerUpToDate && viewData.ledgerId |> Js.Option.isSome;
      <Grid
        title1=("Venture Configuration" |> text)
        area3={
          <div className=ScrollList.containerStyles>
            <MTypography variant=`Title gutterBottom=true>
              ("Policies" |> text)
            </MTypography>
            <MTypography variant=`Body2 gutterBottom=true>
              (
                "The policies determine the threshold at which a proposal will be accepted. This Venture has the following policies:"
                |> text
              )
            </MTypography>
            <MTypography variant=`Body2>
              (
                "Partner addition: "
                ++ policyDescription(viewData.policies.addPartner)
                |> text
              )
            </MTypography>
            <MTypography variant=`Body2>
              (
                "Partner removal: "
                ++ policyDescription(viewData.policies.removePartner)
                |> text
              )
            </MTypography>
            <MTypography variant=`Body2>
              (
                "Payout: "
                ++ policyDescription(viewData.policies.payout)
                |> text
              )
            </MTypography>
            <MTypography variant=`Title gutterTop=true gutterBottom=true>
              ("Wallet" |> text)
            </MTypography>
            (
              switch (viewData.accountSettings.sequence) {
              | Some(nBlocks) =>
                ReasonReact.array([|
                  <MTypography variant=`Body2>
                    ("Degrading multisig is enabled." |> text)
                  </MTypography>,
                  <MTypography variant=`Body2 gutterBottom=true>
                    (
                      "The unlock time is "
                      ++ string_of_int(nBlocks)
                      ++ " blocks (approximately "
                      ++ string_of_int(nBlocks / 144)
                      ++ " days)."
                      |> text
                    )
                  </MTypography>,
                |])
              | None =>
                <MTypography variant=`Body2>
                  ("Degrading multisig is disabled." |> text)
                </MTypography>
              }
            )
            <MTypography variant=`Body2 gutterBottom=true>
              (
                "Here is an overview of the required signatures depending on the number of Custodians backing an address:"
                |> text
              )
            </MTypography>
            <ScrollList>
              MaterialUi.(
                <Table>
                  <TableHead>
                    <TableRow key="header">
                      <TableCell>
                        <MTypography variant=`Body2>
                          ("Number of Partners" |> text)
                        </MTypography>
                      </TableCell>
                      <TableCell>
                        <MTypography variant=`Body2>
                          ("Signatures Required" |> text)
                        </MTypography>
                      </TableCell>
                    </TableRow>
                  </TableHead>
                  <TableBody> nSigs </TableBody>
                </Table>
              )
            </ScrollList>
          </div>
        }
        area4={
          <div>
            <MTypography variant=`Title gutterBottom=true>
              ("Hardware Wallet Integration" |> text)
            </MTypography>
            <MTypography variant=`Body2 gutterBottom=true>
              (ledgerIntegrater |> text)
            </MTypography>
            <MTypography variant=`Body2>
              ("Key status: " |> text)
              <span className=(needsKeyRotation ? Styles.atRiskKeyStatus : "")>
                (keyStatus |> text)
              </span>
            </MTypography>
            (
              viewData.ledgerUpToDate && viewData.ledgerId |> Js.Option.isSome ?
                ReasonReact.null :
                <SingleActionButton
                  onSubmit=executeSubmit
                  canSubmitAction=true
                  withConfirmation=false
                  action=CommandExecutor.Status.SubmitKeys
                  buttonText="Submit public keys"
                  cmdStatus
                />
            )
          </div>
        }
      />;
    },
  };
};
