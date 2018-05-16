open WalletTypes;

let text = Utils.text;

type state = {address: option(string)};

type action =
  | UpdateAddress(string)
  | GetIncomeAddress;

let component = ReasonReact.reducerComponent("Payout");

let make = (~commands: VentureWorkerClient.Cmd.t, _children) => {
  ...component,
  initialState: () => {address: None},
  didMount: ({send}) => send(GetIncomeAddress),
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
  render: ({send, state}) =>
    <div>
      <TitleBar titles=["Create A Payout"] />
      (Utils.text("hello"))
    </div>,
};
