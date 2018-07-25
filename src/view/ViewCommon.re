let text = ReasonReact.string;

let extractString = event => ReactDOMRe.domElementToObj(
                               ReactEventRe.Form.target(event),
                             )##value;

let ignoreEvent = (fn, event) => {
  ReactEventRe.Synthetic.preventDefault(event);
  fn();
};
