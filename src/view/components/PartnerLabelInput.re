let text = ReasonReact.stringToElement;

type action =
  | ChangeLabelId(string)
  | Submit;

type state = {
  labelId: string,
  submit: (~labelId: PrimitiveTypes.LabelId.t) => unit
};

let extractLabelId = event => ReactDOMRe.domElementToObj(
                                ReactEventRe.Form.target(event)
                              )##value;

let component = ReasonReact.reducerComponent("ContributionInput");

let make = (~submit, _children) => {
  ...component,
  initialState: () => {labelId: "", submit},
  reducer: (action, state) =>
    switch action {
    | ChangeLabelId(labelId) => ReasonReact.Update({...state, labelId})
    | Submit =>
      ReasonReact.UpdateWithSideEffects(
        {...state, labelId: ""},
        (
          (_) =>
            state.submit(
              ~labelId=PrimitiveTypes.LabelId.fromString(state.labelId)
            )
        )
      )
    },
  render: ({send, state}) =>
    <div>
      (text("LabelId"))
      <input
        value=state.labelId
        onChange=(e => send(ChangeLabelId(extractLabelId(e))))
      />
      <button onClick=(_e => send(Submit))> (text("Propose Label")) </button>
    </div>
};
