[@bs.module "react-jss/lib/JssProvider"]
external reactClass : ReasonReact.reactClass = "default";

[@bs.module "../assets/js/jss-insertion-point"]
external jss : unit => Js.t({..}) = "default";

[@bs.module "material-ui/styles"]
external createGenerateClassName : unit => Js.t({..}) = "";

let make = children =>
  ReasonReact.wrapJsForReason(
    ~reactClass,
    ~props={
      "jss": () |> jss,
      "generateClassName": () |> createGenerateClassName,
    },
    children,
  );
