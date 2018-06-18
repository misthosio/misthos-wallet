[@bs.module]
external autosuggest : ReasonReact.reactClass = "react-autosuggest";

/* [@bs.deriving abstract] */
/* type jsProps('a, 's, 'p) = { */
/*   suggestions: array('s), */
/*   onSuggestionsFetchRequested: {. "value": string} => unit, */
/*   onSuggestionsClearRequested: unit => unit, */
/*   getSuggestionValue: 's => string, */
/*   renderSuggestion: 's => ReasonReact.reactElement, */
/*   renderSuggestionsContainer: */
/*     { */
/*       . */
/*       "containerProps": 'a, */
/*       "children": ReasonReact.reactElement, */
/*     } => */
/*     ReasonReact.reactElement, */
/*   renderInputComponent: 'p => ReasonReact.reactElement, */
/*   inputProps: 'p, */
/* }; */

let make =
    (
      ~suggestions,
      ~onSuggestionsFetchRequested,
      ~onSuggestionsClearRequested,
      ~getSuggestionValue,
      ~renderSuggestion,
      ~renderSuggestionsContainer,
      ~renderInputComponent,
      ~inputProps,
      _children,
    ) =>
  ReasonReact.wrapJsForReason(
    ~reactClass=autosuggest,
    ~props={
      "suggestions": suggestions,
      "onSuggestionsFetchRequested": onSuggestionsFetchRequested,
      "onSuggestionsClearRequested": onSuggestionsClearRequested,
      "getSuggestionValue": getSuggestionValue,
      "renderSuggestion": renderSuggestion,
      "renderSuggestionsContainer": renderSuggestionsContainer,
      "renderInputComponent": renderInputComponent,
      "inputProps": inputProps,
    },
    _children,
  );
