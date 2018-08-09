[@bs.module]
external autosuggest : ReasonReact.reactClass = "react-autosuggest";

let make =
    (
      ~theme,
      ~suggestions,
      ~onSuggestionsFetchRequested,
      ~onSuggestionsClearRequested,
      ~getSuggestionValue,
      ~shouldRenderSuggestions,
      ~renderSuggestion,
      ~renderSuggestionsContainer,
      ~renderInputComponent,
      ~inputProps,
      _children,
    ) =>
  ReasonReact.wrapJsForReason(
    ~reactClass=autosuggest,
    ~props={
      "theme": theme,
      "suggestions": suggestions,
      "onSuggestionsFetchRequested": onSuggestionsFetchRequested,
      "onSuggestionsClearRequested": onSuggestionsClearRequested,
      "shouldRenderSuggestions": shouldRenderSuggestions,
      "getSuggestionValue": getSuggestionValue,
      "renderSuggestion": renderSuggestion,
      "renderSuggestionsContainer": renderSuggestionsContainer,
      "renderInputComponent": renderInputComponent,
      "inputProps": inputProps,
    },
    _children,
  );
