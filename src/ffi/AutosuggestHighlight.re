type matches;

[@bs.module]
external match_: (string, string) => matches = "autosuggest-highlight/match";
type part = {
  .
  "text": string,
  "highlight": bool,
};

[@bs.module]
external parse: (string, matches) => array(part) =
  "autosuggest-highlight/parse";
