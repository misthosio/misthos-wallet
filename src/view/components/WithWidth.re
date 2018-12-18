[@bs.module "./WithWidthNative"]
external withWidthNative: ReasonReact.reactClass = "default";

[@bs.deriving jsConverter]
type breakPoint = [
  | [@bs.as "xs"] `XS
  | [@bs.as "sm"] `SM
  | [@bs.as "md"] `MD
  | [@bs.as "lg"] `LG
  | [@bs.as "xl"] `XL
];

[@bs.deriving abstract]
type jsProps = {
  breakPoint: string,
  beforeBreak: ReasonReact.reactElement,
  afterBreak: ReasonReact.reactElement,
};

let make = (~breakPoint, ~beforeBreak, ~afterBreak, _children) =>
  ReasonReact.wrapJsForReason(
    ~reactClass=withWidthNative,
    ~props=
      jsProps(
        ~breakPoint=breakPoint |> breakPointToJs,
        ~beforeBreak,
        ~afterBreak,
      ),
    [||],
  );
