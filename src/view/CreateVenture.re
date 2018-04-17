type status =
  | None
  | CreatingVenture(string);

type state = {newVenture: string};

type action =
  | ChangeNewVenture(string)
  | CreateVenture;

let component = ReasonReact.reducerComponent("CreateVenture");

let formText = event => ReactDOMRe.domElementToObj(
                          ReactEventRe.Form.target(event),
                        )##value;

let make = (~onCreateVenture, _children) => {
  ...component,
  initialState: () => {newVenture: ""},
  reducer: (action, state) =>
    switch (action) {
    | ChangeNewVenture(text) => ReasonReact.Update({newVenture: text})
    | CreateVenture =>
      switch (String.trim(state.newVenture)) {
      | "" => ReasonReact.NoUpdate
      | name =>
        ReasonReact.UpdateWithSideEffects(
          {newVenture: ""},
          ((_) => onCreateVenture(name)),
        )
      }
    },
  render: ({send, state}) =>
    <div>
      <input
        placeholder="Create new Venture"
        value=state.newVenture
        onChange=(e => send(ChangeNewVenture(formText(e))))
        autoFocus=Js.true_
      />
      <button onClick=(_e => send(CreateVenture))>
        (ReasonReact.stringToElement("create"))
      </button>
    </div>,
};
