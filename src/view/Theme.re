type t;

[@bs.module "material-ui/styles"]
external createMuiTheme : Js.t({..}) => t = "";

external toJsUnsafe : t => Js.t({..}) = "%identity";

let theme = createMuiTheme({
              "palette": {
                "primary": {
                  "main": "#BEF6E9",
                },
              },
            });
