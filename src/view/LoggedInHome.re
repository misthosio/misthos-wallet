include ViewCommon;

open PrimitiveTypes;

open WalletTypes;

let component = ReasonReact.statelessComponent("LoggedInHome");

let commands: CommandExecutor.commands = {
  reset: () => (),
  proposePartner: (~prospectId: userId) => (),
  endorsePartner: (~processId: processId) => (),
  rejectPartner: (~processId: processId) => (),
  proposePartnerRemoval: (~partnerId: userId) => (),
  endorsePartnerRemoval: (~processId: processId) => (),
  rejectPartnerRemoval: (~processId: processId) => (),
  proposePayout:
    (
      ~accountIdx: accountIdx,
      ~destinations: list((string, BTC.t)),
      ~fee: BTC.t,
    ) =>
    (),
  endorsePayout: (~processId: processId) => (),
  rejectPayout: (~processId: processId) => (),
};

let make = (~index, _children) => {
  ...component,
  render: (_) =>
    <Body2
      titles=["My Ventures"]
      body1=
        <div>
          <CreatePayoutModal
            viewData=(IncomeVenture.viewModel |> ViewModel.createPayoutModal)
            commands
            cmdStatus=Idle
          />
        </div>
      body2=ReasonReact.null
    />,
};
