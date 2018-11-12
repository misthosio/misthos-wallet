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
  open BreakPoints;
  let alignCenter =
    style([display(`flex), flexDirection(column), alignItems(`center)]);
  let addressImage =
    style([sm([height(px(250))]), xs([height(px(200))])]);
  let spinner =
    style([
      sm([height(px(297))]),
      xs([height(px(259))]),
      display(`flex),
      flexDirection(`column),
      alignItems(center),
      justifyContent(`spaceAround),
    ]);
};

let make = (~commands: VentureWorkerClient.Cmd.t, _children) => {
  ...component,
  initialState: () => {address: None},
  didMount: ({onUnmount, send}) => {
    send(GetIncomeAddress);
    let clipboard = Clipboard.make(".copy-btn", "modal");
    onUnmount(() => Clipboard.destroy(clipboard));
  },
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
      | Testnet => Some(WarningsText.testnet)
      | _ => None
      };

    let copyButtonSM =
      state.address
      |> Utils.mapOption(address => {
           let button =
             ReasonReact.cloneElement(
               <MaterialUi.IconButton className="copy-btn">
                 Icons.copy
               </MaterialUi.IconButton>,
               ~props={"data-clipboard-text": address},
               [||],
             );
           <MaterialUi.Tooltip
             id="address-copy-btn"
             title=("Copy to Clipboard" |> text)
             placement=`Bottom>
             button
           </MaterialUi.Tooltip>;
         })
      |> Js.Option.getWithDefault(ReasonReact.null);
    let copyButtonXS =
      state.address
      |> Utils.mapOption(address =>
           ReasonReact.cloneElement(
             <MaterialUi.Button className="copy-btn">
               (text("Copy Address"))
             </MaterialUi.Button>,
             ~props={"data-clipboard-text": address},
             [||],
           )
         )
      |> Js.Option.getWithDefault(ReasonReact.null);

    <Grid
      ?warning
      title1=("Receive BTC" |> text)
      area3={
        <div
          className=(ScrollList.containerStyles ++ " " ++ Styles.alignCenter)>
          <ScrollList>
            <div className=Styles.alignCenter>
              (
                switch (state.address) {
                | Some(address) =>
                  <img
                    className=Styles.addressImage
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
              <WithWidth
                breakPoint=`SM
                beforeBreak={
                  <MTypography variant=`Body2>
                    (state.address |> Js.Option.getWithDefault("") |> text)
                    copyButtonSM
                  </MTypography>
                }
                afterBreak=(
                  [|
                    <MTypography variant=`Body2>
                      (state.address |> Js.Option.getWithDefault("") |> text)
                    </MTypography>,
                    copyButtonXS,
                  |]
                  |> ReasonReact.array
                )
              />
              <MButton onClick=(_e => send(GetIncomeAddress))>
                (text("Generate new address"))
              </MButton>
            </div>
          </ScrollList>
        </div>
      }
    />;
  },
};
