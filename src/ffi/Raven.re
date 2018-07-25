let environment = Environment.get().monitoringEnvironment;
let release = "dev";

type t;

[@bs.module] [@bs.val] external get : t = "raven-js";

type extraConfig = {. "environment": string};

[@bs.send.pipe: t] external config : (string, extraConfig) => t = "";
[@bs.send] external install : t => t = "";

[@bs.send.pipe: t]
external _captureException : 'a => unit = "captureException";

let initialize = () =>
  get
  |> config(
       "https://cb163cef7ce04d4e97641c77cc1b7802@sentry.io/1240624",
       {"environment": environment},
     )
  |> install;

let captureException = error => get |> _captureException(error);
