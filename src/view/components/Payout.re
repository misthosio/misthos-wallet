let text = ReasonReact.stringToElement;

type action =
  | ChangeAmount(string)
  | ChangeDestination(string)
  | Submit;

let extractString = event => ReactDOMRe.domElementToObj(
                               ReactEventRe.Form.target(event),
                             )##value;

type state = {
  amount: string,
  destination: string,
};

let component = ReasonReact.reducerComponent("ContributionInput");

let make = (~onSend, _children) => {
  ...component,
  initialState: () => {amount: "0", destination: ""},
  reducer: (action, state) =>
    switch (action) {
    | ChangeAmount(amount) => ReasonReact.Update({...state, amount})
    | ChangeDestination(destination) =>
      ReasonReact.Update({...state, destination})
    | Submit =>
      ReasonReact.UpdateWithSideEffects(
        {amount: "0", destination: ""},
        (
          (_) =>
            onSend([(state.destination, state.amount |> BTC.fromString)])
        ),
      )
    },
  render: ({send, state}) =>
    <div>
      <div>
        <input
          value=state.amount
          onChange=(e => send(ChangeAmount(extractString(e))))
        />
        (text("BTC"))
      </div>
      (text("Destination:"))
      <input
        value=state.destination
        onChange=(e => send(ChangeDestination(extractString(e))))
      />
      <button onClick=(_e => send(Submit))>
        (text("Propose Payout"))
      </button>
    </div>,
};
