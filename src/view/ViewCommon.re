let text = ReasonReact.string;

let extractString = event => ReactEvent.Form.target(event)##value;

let ignoreEvent = (fn, event) => {
  ReactEvent.Synthetic.preventDefault(event);
  fn();
};
