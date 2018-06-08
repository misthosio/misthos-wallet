type t;

[@bs.val] external _localStorage : t = "localStorage";

[@bs.send] [@bs.return nullable]
external _getItem : (t, string) => option(string) = "getItem";

let getItem = key => _getItem(_localStorage, key);

[@bs.send] external _setItem : (t, string, string) => unit = "setItem";

let setItem = (key, value) => _setItem(_localStorage, key, value);

[@bs.send] external _removeItem : (t, string) => unit = "removeItem";

let removeItem = key => _removeItem(_localStorage, key);
