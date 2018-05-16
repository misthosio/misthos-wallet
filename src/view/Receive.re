open WalletTypes;

let text = Utils.text;

type state = {
  selfRemoved: bool,
  address: option(string),
};

type action =
  | UpdateAddress(string)
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
    selfRemoved:
      initialViewModel |> ViewModel.isPartner(session.userId) == false,
    address: None,
  },
  didMount: ({state: {selfRemoved}, send}) =>
    selfRemoved == false ? send(GetIncomeAddress) : (),
  willReceiveProps: ({state}) => {
    ...state,
    selfRemoved:
      initialViewModel |> ViewModel.isPartner(session.userId) == false,
  },
  reducer: (action, state) =>
    switch (state.selfRemoved, action) {
    | (false, GetIncomeAddress) =>
      ReasonReact.UpdateWithSideEffects(
        {...state, address: None},
        (
          ({send}) =>
            commands.exposeIncomeAddress(~accountIdx=AccountIndex.default)
            |> Js.Promise.(
                 then_(address => send(UpdateAddress(address)) |> resolve)
               )
            |> ignore
        ),
      )
    | (_, UpdateAddress(address)) =>
      ReasonReact.Update({...state, address: Some(address)})
    | _ => ReasonReact.NoUpdate
    },
  render: ({send, state}) =>
    <div>
      <TitleBar titles=["Receive BTC"] />
      <div className=Styles.container>
        (
          switch (state.address) {
          | Some(address) =>
            <img
              src=(
                "https://chart.googleapis.com/chart?chs=250x250&cht=qr&chl="
                ++ address
              )
            />
          | None =>
            <Spinner
              text=(state.selfRemoved ? "READ ONLY" : "Generating new address")
            />
          }
        )
        <MTypography variant=`Body2>
          (state.address |> Js.Option.getWithDefault("") |> text)
        </MTypography>
        <MButton onClick=(_e => send(GetIncomeAddress))>
          (text("Generate new income address"))
        </MButton>
      </div>
    </div>,
};
