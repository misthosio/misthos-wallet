type registry;
[@bs.module "react-jss"] [@bs.new]
external makeRegistry : unit => registry = "SheetsRegistry";
[@bs.send] external toString : registry => string = "";

[@bs.module "react-jss/lib/JssProvider"]
external reactClass : ReasonReact.reactClass = "default";

[@bs.module "../assets/js/jss-insertion-point"]
external jss : unit => Js.t({..}) = "default";

[@bs.module "material-ui/styles"]
external createGenerateClassName : unit => Js.t({..}) = "";

let make = (~registry, children) =>
  ReasonReact.wrapJsForReason(
    ~reactClass,
    ~props={
      "registry": registry,
      "generateClassName": () |> createGenerateClassName,
    },
    children,
  );
