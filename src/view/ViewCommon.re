let text = ReasonReact.string;

let extractString = event => ReactDOMRe.domElementToObj(
                               ReactEventRe.Form.target(event),
                             )##value;
