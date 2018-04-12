let text = ReasonReact.stringToElement;

type action =
  | ChangeAmountInteger(int)
  | ChangeAmountFraction(int)
  | ChangeDescription(string)
  | Submit;

type state = {
  amountInteger: int,
  amountFraction: int,
  description: string,
  submit:
    (
      ~amountInteger: int,
      ~amountFraction: int,
      ~currency: string,
      ~description: string
    ) =>
    unit,
};

let extractAmount = event =>
  int_of_string(
    ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value,
  );

let extractDescription = event => ReactDOMRe.domElementToObj(
                                    ReactEventRe.Form.target(event),
                                  )##value;

let component = ReasonReact.reducerComponent("ContributionInput");

let make = (~submit, _children) => {
  ...component,
  initialState: () => {
    amountInteger: 0,
    amountFraction: 0,
    description: "",
    submit,
  },
  reducer: (action, state) =>
    switch (action) {
    | ChangeAmountInteger(amountInteger) =>
      ReasonReact.Update({...state, amountInteger})
    | ChangeAmountFraction(amountFraction) =>
      ReasonReact.Update({...state, amountFraction})
    | ChangeDescription(description) =>
      ReasonReact.Update({...state, description})
    | Submit =>
      ReasonReact.UpdateWithSideEffects(
        {...state, amountInteger: 0, amountFraction: 0, description: ""},
        (
          (_) =>
            state.submit(
              ~amountInteger=state.amountInteger,
              ~amountFraction=state.amountFraction,
              ~currency="USD",
              ~description=state.description,
            )
        ),
      )
    },
  render: ({send, state}) =>
    <div>
      <div>
        <input
          value=(string_of_int(state.amountInteger))
          onChange=(e => send(ChangeAmountInteger(extractAmount(e))))
        />
        (text(","))
        <input
          value=(string_of_int(state.amountFraction))
          onChange=(e => send(ChangeAmountFraction(extractAmount(e))))
        />
        (text("USD"))
      </div>
      (text("Description:"))
      <input
        value=state.description
        onChange=(e => send(ChangeDescription(extractDescription(e))))
      />
      <button onClick=(_e => send(Submit))>
        (text("Propose Contribution"))
      </button>
    </div>,
};
