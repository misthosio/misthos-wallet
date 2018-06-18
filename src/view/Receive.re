include ViewCommon;

open WalletTypes;

[@bs.module] external copy : string = "../assets/img/copy.svg";

type state = {address: option(string)};

type action =
  | UpdateAddress(string)
  | GetIncomeAddress;

let component = ReasonReact.reducerComponent("Receive");

module Styles = {
  open Css;
  let container =
    style([display(`flex), flexDirection(`column), alignItems(`center)]);
  let spinner =
    style([
      height(px(269)),
      display(`flex),
      flexDirection(`column),
      alignItems(center),
      justifyContent(`spaceAround),
    ]);
};

let make = (~commands: VentureWorkerClient.Cmd.t, _children) => {
  ...component,
  initialState: () => {address: None},
  didMount: ({send}) => send(GetIncomeAddress),
  subscriptions: _ => [
    Sub(
      () => Clipboard.make(".copy-btn", "modal"),
      clipboard => clipboard |> Clipboard.destroy,
    ),
  ],
  reducer: (action, _state) =>
    switch (action) {
    | GetIncomeAddress =>
      ReasonReact.UpdateWithSideEffects(
        {address: None},
        (
          ({send}) =>
            commands.exposeIncomeAddress(~accountIdx=AccountIndex.default)
            |> Js.Promise.(
                 then_(address => send(UpdateAddress(address)) |> resolve)
               )
            |> ignore
        ),
      )
    | UpdateAddress(address) => ReasonReact.Update({address: Some(address)})
    },
  render: ({send, state}) => {
    let warning =
      switch (Environment.get().network) {
      | Testnet => Some(Text.Warnings.testnet)
      | _ => None
      };

    let copyButton =
      state.address
      |> Utils.mapOption(address =>
           ReasonReact.cloneElement(
             <MaterialUi.IconButton className="copy-btn">
               Icons.copy
             </MaterialUi.IconButton>,
             ~props={"data-clipboard-text": address},
             [||],
           )
         )
      |> Js.Option.getWithDefault(ReasonReact.null);
    <Grid
      ?warning
      title1=("Receive BTC" |> text)
      area3={
        <div className=Styles.container>
          (
            switch (state.address) {
            | Some(address) =>
              <img
                height="250px"
                src=(
                  "https://chart.googleapis.com/chart?chs=250x250&cht=qr&chl="
                  ++ address
                )
              />
            | None =>
              <Spinner
                className=Styles.spinner
                text="Generating new address"
              />
            }
          )
          <MTypography variant=`Body2>
            (state.address |> Js.Option.getWithDefault("") |> text)
            copyButton
          </MTypography>
          <MButton onClick=(_e => send(GetIncomeAddress))>
            (text("Generate new income address"))
          </MButton>
        </div>
      }
    />;
  },
};
