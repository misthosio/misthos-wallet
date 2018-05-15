open WalletTypes;

let text = Utils.text;

type state = {
  viewModel: ViewModel.t,
  selfRemoved: bool,
};

type action =
  | GetIncomeAddress;

let component = ReasonReact.reducerComponent("Receive");

module Styles = {
  open Css;
  let container =
    style([display(`flex), flexDirection(`column), alignItems(`center)]);
};

let make =
    (
      ~venture as initialViewModel,
      ~session: Session.Data.t,
      ~commands: VentureWorkerClient.Cmd.t,
      _children,
    ) => {
  ...component,
  initialState: () => {
    viewModel: initialViewModel,
    selfRemoved:
      initialViewModel |> ViewModel.isPartner(session.userId) == false,
  },
  willReceiveProps: (_) => {
    viewModel: initialViewModel,
    selfRemoved:
      initialViewModel |> ViewModel.isPartner(session.userId) == false,
  },
  reducer: (action, state) =>
    switch (state.selfRemoved, action) {
    | (false, GetIncomeAddress) =>
      commands.exposeIncomeAddress(~accountIdx=AccountIndex.default);
      ReasonReact.NoUpdate;
    | _ => ReasonReact.NoUpdate
    },
  render: ({send, state}) => {
    let address = List.nth(ViewModel.incomeAddresses(state.viewModel), 0);
    <div>
      <TitleBar titles=["Receive BTC"] />
      <div className=Styles.container>
        <img
          src=(
            "https://chart.googleapis.com/chart?chs=250x250&cht=qr&chl="
            ++ address
          )
        />
        <MTypography variant=`Body2> (address |> text) </MTypography>
        <MButton onClick=(_e => send(GetIncomeAddress))>
          (text("Generate new income address"))
        </MButton>
      </div>
    </div>;
  },
};
