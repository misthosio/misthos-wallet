// Generated by BUCKLESCRIPT VERSION 4.0.5, PLEASE EDIT WITH CARE
'use strict';

var ReasonReact = require("reason-react/src/ReasonReact.js");
var ReactAutosuggest = require("react-autosuggest");

function make(theme, suggestions, onSuggestionsFetchRequested, onSuggestionsClearRequested, getSuggestionValue, shouldRenderSuggestions, renderSuggestion, renderSuggestionsContainer, renderInputComponent, inputProps, _children) {
  return ReasonReact.wrapJsForReason(ReactAutosuggest, {
              theme: theme,
              suggestions: suggestions,
              onSuggestionsFetchRequested: onSuggestionsFetchRequested,
              onSuggestionsClearRequested: onSuggestionsClearRequested,
              shouldRenderSuggestions: shouldRenderSuggestions,
              getSuggestionValue: getSuggestionValue,
              renderSuggestion: renderSuggestion,
              renderSuggestionsContainer: renderSuggestionsContainer,
              renderInputComponent: renderInputComponent,
              inputProps: inputProps
            }, _children);
}

exports.make = make;
/* ReasonReact Not a pure module */
